;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.context
  "Relay hops as context wrappers: env -i, sudo, ssh (direct, ProxyJump,
  script on stdin). Each hop changes WHERE the inner program runs, so a
  wrapper carries :context, the key of the profile its inner layer is
  planned against (see grammar.solve), and :parses, the model of what
  re-parses the inner text on its way (checked by grammar.parse.relay).

  Hop      renders (W = the inner program as ONE outer sh word)    parses of the inner text
  env      env -i PATH=P sh -c W                                  local sh (W), then sh
  sudo     sudo -n [-u U] -- sh -c W                              local sh (W), then sh; sudo execs, no parse
  ssh      ssh -T -o BatchMode=yes [opts] -- DEST W               local sh (W), then DEST's LOGIN shell
  jump     ssh ... -J J1,J2 -- DEST W                             as ssh: jump hosts forward TCP only
  stdin    printf FMT | ssh ... -- DEST sh                        local sh (FMT, octal ASCII); the login
                                                                  shell parses only `sh`; sh reads the
                                                                  program from stdin

  ssh joins its argv with spaces and the remote login shell parses the
  result, so W must denote exactly the inner program and the inner program
  must be POSIX for that login shell: the direct and jump hops require
  :sh/posix-words of their INNER profile (probed through the hop itself).
  csh and fish fail that probe; the stdin hop reaches them anyway, since
  their only parse is of the word `sh`. A nested ssh is two ssh hops: the
  second runs inside the first's remote shell, one more login-shell parse.

  DEST never starts with `-` (refused here) and follows `--`. DEST and the
  options are rendered as literal words but are not data to print: they
  may come from a secret store."
  (:require [clojure.string :as str]
            [grammar.candidate :as c]
            [grammar.lit :as lit]
            [grammar.probe :as probe]
            [grammar.profile :as p]
            [grammar.runtime :as rt]))

(defn- lit-in [env lang s] ((:lit env) lang s))

(defn- words
  "Candidates for fixed literal words followed by the candidate code word."
  [env fixed code]
  (apply c/combine (fn [& ws] (str/join " " ws)) (map #(lit-in env :sh %) (concat fixed [code]))))

(defn- check-dest! [dest]
  (when (or (not (string? dest)) (str/blank? dest) (str/starts-with? dest "-") (re-find #"\s" dest))
    (throw (ex-info "ssh destination must be a non-blank word that does not start with -" {}))))

(def ssh-base ["ssh" "-T" "-o" "BatchMode=yes"])

(defn- reach-cap [route] (keyword "reach" (name route)))

(defn env-hop
  "Wrapper: run the inner sh program under env -i with PATH=path."
  [{:keys [route path] :or {route :env-clean}}]
  {:id (keyword "ctx" (str "sh->env." (name route))) :outer :sh :inner :sh :cost 6
   :context (keyword "env" (name route)) :requires #{:sh.cmd/env}
   :parses [:outer-sh :sh]
   :render (fn [env code] (words env ["env" "-i" (str "PATH=" path) "sh" "-c"] code))})

(defn sudo-hop
  "Wrapper: run the inner sh program as user (default root) via sudo -n."
  [{:keys [route user] :or {route :root}}]
  (let [u (when user ["-u" user])]
    {:id (keyword "ctx" (str "sh->sudo." (name route))) :outer :sh :inner :sh :cost 8
     :context (keyword "sudo" (name route)) :requires #{(reach-cap route)}
     :parses [:outer-sh :sh]
     :render (fn [env code] (words env (concat ["sudo" "-n"] u ["--" "sh" "-c"]) code))}))

(defn ssh-hop
  "Wrapper: run the inner sh program in dest's login shell. opts are extra
  ssh options (e.g. [\"-F\" cfg]); jump, when given, is a -J value and makes
  this a ProxyJump hop."
  [{:keys [route dest opts jump]}]
  (check-dest! dest)
  (let [fixed (concat ssh-base opts (when jump ["-J" jump]) ["--" dest])]
    {:id (keyword "ctx" (str (if jump "sh->ssh-jump." "sh->ssh.") (name route))) :outer :sh :inner :sh
     :cost (if jump 21 20)
     :context (keyword (if jump "ssh-jump" "ssh") (name route)) :requires #{(reach-cap route)}
     :inner-requires #{:sh/posix-words}
     :parses [:outer-sh :login-shell]
     :render (fn [env code] (words env fixed code))}))

(defn ssh-stdin-hop
  "Wrapper: send the inner sh program as a script on stdin to `sh` started
  by dest's login shell, whatever that shell's dialect. The program must
  not read stdin itself."
  [{:keys [route dest opts jump]}]
  (check-dest! dest)
  (let [fixed (concat ssh-base opts (when jump ["-J" jump]) ["--" dest "sh"])]
    {:id (keyword "ctx" (str "sh->ssh-stdin." (name route))) :outer :sh :inner :sh :cost 24
     :context (keyword "ssh-stdin" (name route)) :requires #{(reach-cap route) :sh/pipe :sh/printf}
     :parses [:outer-sh :login-shell-word-sh :sh-stdin]
     :render (fn [env code]
               (apply c/combine (fn [f & ws] (str "printf " f " | " (str/join " " ws)))
                      (lit-in env :sh (lit/printf-format code))
                      (map #(lit-in env :sh %) fixed)))}))

(defn hop-tag
  "The part of a context key one hop contributes: its wrapper id without
  the ctx/ namespace, e.g. sh->ssh-stdin.n2c. Transport and route both
  show, so a direct and a stdin hop to one host never share a profile."
  [w]
  (name (:id w)))

(defn chain
  "Hops ws (outermost first) linked for the planner. Each hop's :context
  becomes a PATH key naming every hop from the root through it, e.g.
  :hop/sh->ssh.n1>sh->sudo.root, so two chains ending in sudo on
  different hosts, or a direct and a stdin hop to one host, never share
  a profile. Each hop's :from is the path before it (nil: the root).
  The planner then reaches the last context only through all of them."
  [ws]
  (first
   (reduce (fn [[acc from tags] w]
             (let [tags (conj tags (hop-tag w))
                   k (keyword "hop" (str/join ">" tags))]
               [(conj acc (assoc w :context k :from from :route-context (:context w))) k tags]))
           [[] nil []]
           ws)))

(defn check-contexts!
  "Refuse wrappers ws in which two context wrappers declare one :context:
  the planner keeps one profile per context."
  [ws]
  (let [dups (keep (fn [[k n]] (when (< 1 n) k)) (frequencies (keep :context ws)))]
    (when (seq dups)
      (throw (ex-info "two hops declare one :context; link them with grammar.context/chain"
                      {:contexts (vec (sort dups))})))
    ws))

(def default-lits
  "Literal encoders a hop renders with when no planner chose them: POSIX
  single quotes for sh, single-quoted str for python."
  {:sh lit/sh-single :python lit/py-single})

(defn render-with
  "The cheapest text of wrapper w around code, literals from lits
  (lang -> encoder), or nil when it cannot carry code."
  ([w code] (render-with w default-lits code))
  ([w lits code]
   (:code (first ((:render w) {:lit (fn [lang s] (c/leaf ((lits lang) s)))} code)))))

(def posix-words-text
  "What :sh/posix-words sends: one printf whose operand holds a newline, !,
  a single quote, $, a lone backslash, a DOUBLE backslash, a backslash
  right before a single quote, a tab and a double space, in single quotes
  and backslash escapes. Each one is there to refute a dialect:
    csh   rejects the newline in quotes and expands !
    fish  turns \\\\ into \\ and \\' into ' inside single quotes, so the
          double backslash shrinks and the backslash-quote ends early
  A shell that passes it treats single quotes as POSIX does, which is all
  a single-quoted hop needs."
  "a\n!b'$c\\d\\\\e\\'f\tg  h")

(def probes
  "Probes of the far side of a hop, run IN the stacked runtime.
    :sh/posix-words  the shell that parsed this program splits and
                     unquotes words like POSIX sh: through a direct ssh
                     hop that is the remote LOGIN shell."
  [{:cap :sh/posix-words :needs [:sh/printf]
    :script (str "printf %s " (lit/sh-single posix-words-text) "\\ \\\\")
    :expect (str posix-words-text " \\")}])

(defn reach-probe
  "Probe, run in the hop's OUTER runtime, that the hop delivers a program
  and returns its output: (:requires w)'s :reach/<route> cap."
  [w]
  {:cap (first (filter #(= "reach" (namespace %)) (:requires w)))
   :needs [:sh/printf :sh/single-quote]
   :script (render-with w "printf %s ok")
   :expect "ok"})

(defn hop-runtime
  "The runtime on the far side of hop w, reached through outer (a runtime
  whose language is (:outer w)). Its id is (:context w), so discovered
  evidence reads ev.<context>/..., and its profile belongs under that key
  of a planning env's :profiles."
  ([outer w] (hop-runtime outer w default-lits))
  ([outer w lits]
   (rt/stacked-runtime {:id (keyword (str (namespace (:context w)) "." (name (:context w))))
                        :lang (:inner w)
                        :outer outer
                        :wrap #(render-with w lits %)})))

(defn chain-runtime
  "The runtime at the end of hops ws, outermost first, from root."
  [root ws]
  (reduce hop-runtime root ws))

(defn probe-reach
  "profile (of runtime rt, the near side of hop w) with w's reach cap
  probed through rt. Skipped (:unknown) unless its needs are :yes."
  [rt profile w]
  (let [{:keys [needs] :as pr} (reach-probe w)
        unproven (remove #(= :yes (p/verdict profile %)) needs)]
    (p/add-evidence profile
                    (if (seq unproven)
                      (probe/skip-evidence (rt/runtime-id rt) pr profile unproven)
                      (probe/run-evidence (rt/runtime-id rt) pr (rt/execute rt (:script pr)))))))

(defn discover-chain!
  "Profile root and every far side of the linked hops ws (see chain).
  discover is (fn [runtime] profile), e.g. grammar.core/discover!. Each
  near side also gets its hop's :reach/<route> cap. A hop whose reach is
  not :yes is not crossed: its far side and everything behind it stay
  without a profile, so the planner answers :unknown for them.
  Returns {:root profile :profiles {context profile} :runtimes {context rt}}."
  [discover root ws]
  (loop [rt root prof (discover root) near nil hs ws acc {:profiles {} :runtimes {}}]
    (let [store (fn [acc prof] (if near (assoc-in acc [:profiles near] prof) (assoc acc :root prof)))]
      (if-let [w (first hs)]
        (let [prof (probe-reach rt prof w)
              reach (first (filter #(= "reach" (namespace %)) (:requires w)))
              acc (store acc prof)]
          (if (= :yes (p/verdict prof reach))
            (let [far (hop-runtime rt w)]
              (recur far (discover far) (:context w) (rest hs) (assoc-in acc [:runtimes (:context w)] far)))
            acc))
        (store acc prof)))))

(def module
  "Relay hops as a grammar.core language module: no realizations, no fixed
  wrappers (hops are built per destination), the far-side probes."
  {:id :context :realizations [] :wrappers [] :probes #'probes})
