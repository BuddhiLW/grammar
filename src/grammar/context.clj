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
            [grammar.lit :as lit]))

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
     :context (keyword "ssh" (name route)) :requires #{(reach-cap route)}
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
     :context (keyword "ssh" (name route)) :requires #{(reach-cap route) :sh/pipe :sh/printf}
     :parses [:outer-sh :login-shell-word-sh :sh-stdin]
     :render (fn [env code]
               (apply c/combine (fn [f & ws] (str "printf " f " | " (str/join " " ws)))
                      (lit-in env :sh (lit/printf-format code))
                      (map #(lit-in env :sh %) fixed)))}))
