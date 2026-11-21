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

(defn probe-lang
  "Language a probe's script is written in: its :lang, :sh when absent.
  discover runs a probe only in a runtime of that language."
  [probe]
  (:lang probe :sh))

(def os-functions
  "The os functions the python realizations reach: pathlib's mkdir calls
  os.mkdir, its unlink calls os.unlink. Each has a fixed :py.os/<f> probe."
  ["mkdir" "unlink"])

(defn- os-fn-probe [f]
  {:cap (keyword "py.os" f) :lang :python :needs [:py/sys]
   :script (str "import os,sys;sys.stdout.write(\"ok\" if callable(getattr(os," (lit/py-double f)
                ",None)) else \"no\")")
   :expect "ok"})

(def py-probes
  "Probes written in python, for a runtime whose language is :python
  (python3 -c). Ordered like `probes`: every :needs is probed earlier in
  this list. The scripts hold no single quote, so each one lifts into an sh
  probe unchanged (see python-via-sh).

    :py/sys            import sys and sys.stdout.write work
    :py/stdout-buffer  bytes written to sys.stdout.buffer arrive unchanged:
                       NUL, a two-byte UTF-8 sequence and CR LF included
    :py/pathlib        import pathlib
    :py/subprocess     import subprocess
    :py/urllib         import shutil, urllib.request
    :py.os/<f>         os.<f> exists and is callable, for f in os-functions
    :interp/sh         subprocess runs [\"sh\" \"-c\" ...], sh found on PATH:
                       the argv :ctx/python->sh runs. A /bin/sh off PATH does
                       not count. Marked :lift false: sh runtimes prove
                       :interp/sh with their own probe."
  (-> [{:cap :py/sys :lang :python :needs []
        :script "import sys;sys.stdout.write(\"ok\")" :expect "ok"}
       {:cap :py/stdout-buffer :lang :python :needs []
        :script "import sys;b=sys.stdout.buffer;b.write(bytes([111,0,195,169,13,10,107]));b.flush()"
        :expect "o\u0000\u00e9\r\nk"}
       {:cap :py/pathlib :lang :python :needs [:py/sys]
        :script "import pathlib,sys;sys.stdout.write(\"ok\")" :expect "ok"}
       {:cap :py/subprocess :lang :python :needs [:py/sys]
        :script "import subprocess,sys;sys.stdout.write(\"ok\")" :expect "ok"}
       {:cap :py/urllib :lang :python :needs [:py/sys]
        :script "import shutil,sys,urllib.request;sys.stdout.write(\"ok\")" :expect "ok"}]
      (into (map os-fn-probe) os-functions)
      (conj {:cap :interp/sh :lang :python :lift false :needs [:py/sys :py/subprocess]
             :script (str "import subprocess,sys\n"
                          "try:o=subprocess.run([\"sh\",\"-c\",\"printf %s ok\"],stdin=subprocess.DEVNULL,stdout=subprocess.PIPE).stdout\n"
                          "except OSError:o=b\"no\"\n"
                          "sys.stdout.write(o.decode(\"utf-8\",\"replace\"))")
             :expect "ok"})))

(defn- python-law?
  "True for a python probe whose law is about python itself, so that an sh
  runtime can prove it of the python3 it reaches."
  [probe]
  (not (false? (:lift probe))))

(defn python-via-sh
  "The sh probe for python probe pr: its script run by `python3 -c`, one
  single-quoted word, needing what pr needs plus :interp/python3 and
  :sh/single-quote. The cap is pr's own, so :py/pathlib proved here and
  :py/pathlib proved by a python runtime state one law."
  [{:keys [needs script] :as pr}]
  (-> pr
      (dissoc :lang :lift)
      (assoc :needs (into [:interp/python3 :sh/single-quote] (remove #{:interp/python3 :sh/single-quote}) needs)
             :script (str "python3 -c " (lit/sh-single script)))))

(def probes
  "The sh probes: scripts for a runtime whose language is :sh. Ordered so
  that every :needs is probed before the probes that rely on it. Probe
  scripts avoid output redirection on purpose: a restricted shell must
  still be able to answer every other question.

  The :py/ and :py.os/ probes are not written here: each is a py-probe
  run as `python3 -c '<the python probe>'`, so a :py/ cap names the same
  law whether a python runtime proved it itself or a shell proved it of
  the python3 it reaches."
  (-> [{:cap :sh/printf       :needs []
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
        :script "python3 -c 'print(\"ok\",end=\"\")'" :expect "ok"}]
      (into (comp (filter python-law?) (map python-via-sh)) py-probes)
      (conj {:cap :sh.wget/gnu     :needs [:sh/printf :sh/single-quote]
             :doc "The wget the shell resolves is GNU Wget, which verifies TLS certificates. busybox wget rejects --version, so it prints no; so does a shell with no wget."
             :script "case $(wget --version) in 'GNU Wget'*) printf %s ok;; *) printf %s no;; esac" :expect "ok"})))

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

(def catalog
  "Every built-in probe, sh then python: what grammar.core's :core module
  composes. discover! keeps the ones written in the runtime's language."
  (into probes py-probes))

(defn for-lang
  "The probes of ps whose script is written in lang, in order."
  [lang ps]
  (filterv #(= lang (probe-lang %)) ps))

(defn discover
  "Run every probe through run! (fn [script] -> {:argv :exit :out :err} or
  {:argv :error}), in order, folding the evidence into a fresh profile.
  probes are run as given, whatever their :lang; pick them with for-lang.
  The 2-arity runs the sh probes."
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
      ["bin" :python] {:cap cap :lang :python :needs [:py/sys]
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
