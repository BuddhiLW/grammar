;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.probe
  "Discovery. A probe is a law stated as a tiny script with an expected
  stdout, run in the runtime under study.

    printed exactly :expect           -> :yes  (law holds here)
    ran, printed anything else        -> :no   (law refuted here)
    never ran (launch error, timeout) -> :unknown
    prerequisite (:needs) not :yes    -> :unknown, probe skipped

  Every verdict becomes Evidence carrying the script, exit status and
  output, so any later choice can be traced back to what was observed.
  This namespace is pure: the runner is passed in."
  (:require [clojure.string :as str]
            [grammar.profile :as p]
            [grammar.lit :as lit]))

(def probes
  "Ordered so that every :needs is probed before the probes that rely on it.
  Probe scripts avoid output redirection on purpose: a restricted shell
  must still be able to answer every other question."
  [{:cap :sh/printf       :needs []
    :script "printf %s ok" :expect "ok"}
   {:cap :sh/single-quote :needs [:sh/printf]
    :script "printf %s 'o''k'" :expect "ok"}
   {:cap :sh/double-quote :needs [:sh/printf]
    :script "printf %s \"o\"\"k\"" :expect "ok"}
   {:cap :sh/ansi-c-quote :needs [:sh/printf]
    :script "printf %s $'\\157'k" :expect "ok"}
   {:cap :sh/redirect     :needs [:sh/printf :sh/single-quote]
    :script "eval ': >/dev/null' && printf %s ok" :expect "ok"}
   {:cap :sh/pipe         :needs [:sh/printf :sh/single-quote]
    :script "printf 'ok\\n' | { read -r x; printf %s \"$x\"; }" :expect "ok"}
   {:cap :bin/tee         :needs [:sh/printf :sh/pipe]
    :script "printf %s ok | tee" :expect "ok"}
   {:cap :interp/sh       :needs [:sh/printf :sh/single-quote]
    :script "sh -c 'printf %s ok'" :expect "ok"}
   {:cap :interp/python3  :needs [:sh/single-quote]
    :script "python3 -c 'print(\"ok\",end=\"\")'" :expect "ok"}
   {:cap :py/sys          :needs [:interp/python3 :sh/single-quote]
    :script "python3 -c 'import sys;sys.stdout.write(\"ok\")'" :expect "ok"}
   {:cap :py/pathlib      :needs [:interp/python3 :sh/single-quote]
    :script "python3 -c 'import pathlib,sys;sys.stdout.write(\"ok\")'" :expect "ok"}
   {:cap :py/subprocess   :needs [:interp/python3 :sh/single-quote]
    :script "python3 -c 'import subprocess,sys;sys.stdout.write(\"ok\")'" :expect "ok"}
   {:cap :py/urllib       :needs [:interp/python3 :sh/single-quote]
    :script "python3 -c 'import shutil,sys,urllib.request;sys.stdout.write(\"ok\")'" :expect "ok"}])

(defn judge
  "Verdict of one probe run. A run that never happened proves nothing."
  [{:keys [expect]} {:keys [out error]}]
  (cond error :unknown
        (= out expect) :yes
        :else :no))

(defn- clip [s] (when s (if (> (count s) 400) (str (subs s 0 400) "...") s)))

(defn evidence-id [runtime-id cap]
  (keyword (str "ev." (name runtime-id)) (str (namespace cap) "." (name cap))))

(defn run-evidence
  "Evidence for a probe that ran (or tried to)."
  [runtime-id {:keys [cap script expect] :as probe} result]
  (cond-> {:id (evidence-id runtime-id cap)
           :kind :probe :cap cap :runtime runtime-id
           :verdict (judge probe result)
           :script script :expect expect
           :exit (:exit result)}
    (:argv result) (assoc :argv (vec (:argv result)))
    (:out result) (assoc :out (clip (:out result)))
    (:err result) (assoc :err (clip (:err result)))
    (:error result) (assoc :error (:error result))))

(defn skip-evidence
  "Evidence for a probe not run because a prerequisite was not proven."
  [runtime-id {:keys [cap script expect]} profile unproven]
  {:id (evidence-id runtime-id cap)
   :kind :probe :cap cap :runtime runtime-id :verdict :unknown
   :script script :expect expect
   :reason (str "skipped: prerequisite not proven: "
                (str/join ", " (map #(str % "=" (name (p/verdict profile %))) unproven)))})

(defn- step
  "Run one probe through run! unless a prerequisite is unproven, and fold
  the resulting evidence into profile."
  [runtime-id run! profile {:keys [needs] :as probe}]
  (let [unproven (vec (remove #(= :yes (p/verdict profile %)) needs))]
    (p/add-evidence profile
                    (if (seq unproven)
                      (skip-evidence runtime-id probe profile unproven)
                      (run-evidence runtime-id probe (run! (:script probe)))))))

(defn discover
  "Run every probe through run! (fn [script] -> {:argv :exit :out :err} or
  {:argv :error}), in order, folding the evidence into a fresh profile."
  ([runtime-id run!] (discover runtime-id run! probes))
  ([runtime-id run! probes]
   (reduce (partial step runtime-id run!) (p/empty-profile runtime-id) probes)))

(defn- sh-path-search
  "sh script printing ok when quoted program q names an executable file:
  the file itself when it holds a slash, else a PATH entry. Builtins and
  functions are not consulted, so the answer matches exec, not the shell."
  [prog q]
  (if (str/includes? prog "/")
    (str "if [ -f " q " ] && [ -x " q " ]; then printf %s ok; else printf %s no; fi")
    (str "r=no; IFS=:; for d in $PATH; do if [ -f \"$d/\"" q " ] && [ -x \"$d/\"" q " ]; then r=ok; fi; done; printf %s \"$r\"")))

(defn on-demand
  "Probe for a capability the fixed catalog cannot list ahead of time, in
  the runtime's language, or nil.
    :bin/<prog>     exec finds prog: an executable file on PATH (or prog
                    itself when it holds a slash). Builtins do not count.
    :sh.cmd/<prog>  the shell resolves the word prog (command -v): PATH,
                    builtins and applets all count. sh runtimes only."
  [lang cap]
  (let [prog (name cap)]
    (case [(namespace cap) lang]
      ["bin" :sh] (when-let [q (lit/sh-single prog)]
                    {:cap cap :needs [:sh/printf :sh/single-quote :sh/double-quote]
                     :script (sh-path-search prog q)
                     :expect "ok"})
      ["bin" :python] {:cap cap :needs []
                       :script (str "import shutil,sys;sys.stdout.write('ok' if shutil.which("
                                    (lit/py-single prog) ") else 'no')")
                       :expect "ok"}
      ["sh.cmd" :sh] (when-let [q (lit/sh-single prog)]
                       {:cap cap :needs [:sh/printf :sh/single-quote]
                        :script (str "case $(command -v " q ") in '') printf %s no;; *) printf %s ok;; esac")
                        :expect "ok"})
      nil)))

(defn resolve-caps
  "Probe every cap in caps that is still :unknown and has an on-demand probe
  in lang, folding the evidence into profile. Other caps are left as they are."
  [profile runtime-id lang run! caps]
  (reduce (fn [prof cap]
            (if-let [pr (and (= :unknown (p/verdict prof cap)) (on-demand lang cap))]
              (step runtime-id run! prof pr)
              prof))
          profile
          caps))
