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

(defn- py-lines
  "One python program from its lines."
  [& lines]
  (str/join "\n" lines))

(def ^:private py-scratch
  "python statement binding p to a fresh path under $TMPDIR (else /tmp),
  named by pid and random bytes. tempfile is not used on purpose: its
  gettempdir writes a test file, so a runtime that refuses writes would
  refute mkdir too."
  (str "p=pathlib.Path(os.environ.get(\"TMPDIR\") or \"/tmp\","
       "\"grammar-probe-%d-%s\"%(os.getpid(),os.urandom(4).hex()))"))

(defn- with-cleanup
  "python lines running body (lines), then always the cleanup statement:
  a cleanup that raises is written to stderr, naming the path it left,
  so the evidence shows what the probe could not remove. The verdict
  stays the law's: a refused cleanup is not a refuted write."
  [cleanup & body]
  (concat ["try:"] (map #(str " " %) body)
          ["finally:"
           (str " try:" cleanup)
           " except Exception as c:sys.stderr.write(\"cleanup failed, left %s: %r\\n\"%(p,c))"]))

(defn- guarded
  "python program that imports sys, then runs body (lines) after importing
  modules. Anything raised in there (a missing module, an OSError, a
  refusal from an audit hook) prints no, with the exception on stderr: the
  program ran and the law was refuted. Only a program that never ran is
  :unknown."
  [modules & body]
  (apply py-lines "import sys" "try:" (str " import " modules)
         (concat (map #(str " " %) body)
                 ["except Exception as e:sys.stderr.write(repr(e));sys.stdout.write(\"no\")"])))

(def py-probes
  "Probes written in python, for a runtime whose language is :python
  (python3 -c). Ordered like `probes`: every :needs is probed earlier in
  this list. A probe performs the operation its law is about, never just an
  import: a runtime that refuses it (a missing module, an OSError, an audit
  hook raising on subprocess, socket or a write-mode open) prints no. Each
  script lifts into an sh probe (see python-via-sh).

    :py/sys            import sys and sys.stdout.write work
    :py/stdout-buffer  bytes written to sys.stdout.buffer arrive unchanged:
                       NUL, a two-byte UTF-8 sequence and CR LF included
    :py/pathlib        import pathlib
    :py/open-write     pathlib writes a file under $TMPDIR (else /tmp) and
                       reads the same bytes back
    :py.os/mkdir       pathlib.mkdir (os.mkdir) makes a directory there
    :py.os/unlink      pathlib.unlink (os.unlink) removes a symlink made
                       there, so it needs no write-mode open; a runtime that
                       refuses os.symlink refutes it too
    :py/subprocess     subprocess runs sys.executable as a child and reads
                       its stdout
    :py/urllib         urllib.request fetches a body from a server this
                       script listens with on 127.0.0.1
    :interp/sh         subprocess runs [\"sh\" \"-c\" ...], sh found on PATH:
                       the argv :ctx/python->sh runs. With PATH set, a
                       /bin/sh off PATH does not count; with PATH unset
                       python searches os.defpath, as the wrapper does.
                       Marked :lift false: sh runtimes prove :interp/sh with
                       their own probe."
  [{:cap :py/sys :lang :python :needs []
    :script "import sys;sys.stdout.write(\"ok\")" :expect "ok"}
   {:cap :py/stdout-buffer :lang :python :needs [:py/sys]
    :script "import sys;b=sys.stdout.buffer;b.write(bytes([111,0,195,169,13,10,107]));b.flush()"
    :expect "o\u0000\u00e9\r\nk"}
   {:cap :py/pathlib :lang :python :needs [:py/sys]
    :script "import pathlib,sys;sys.stdout.write(\"ok\")" :expect "ok"}
   {:cap :py/open-write :lang :python :needs [:py/sys :py/pathlib]
    :script (apply guarded "os,pathlib" py-scratch
                   (with-cleanup "p.unlink(missing_ok=True)"
                     "p.write_bytes(b\"ok\")" "sys.stdout.write(p.read_bytes().decode())"))
    :expect "ok"}
   {:cap :py.os/mkdir :lang :python :needs [:py/sys :py/pathlib]
    :script (apply guarded "os,pathlib" py-scratch
                   (with-cleanup "os.path.isdir(p) and p.rmdir()"
                     "p.mkdir()" "sys.stdout.write(\"ok\" if p.is_dir() else \"no\")"))
    :expect "ok"}
   {:cap :py.os/unlink :lang :python :needs [:py/sys :py/pathlib]
    :script (apply guarded "os,pathlib" py-scratch
                   (with-cleanup "os.path.lexists(p) and p.unlink()"
                     "os.symlink(\"grammar-probe-target\",p)" "p.unlink()"
                     "sys.stdout.write(\"no\" if os.path.lexists(p) else \"ok\")"))
    :expect "ok"}
   {:cap :py/subprocess :lang :python :needs [:py/sys]
    :script (guarded "subprocess"
                     (str "o=subprocess.run([sys.executable,\"-c\",\"import sys;sys.stdout.write(sys.argv[1])\",\"ok\"],"
                          "stdout=subprocess.PIPE).stdout")
                     "sys.stdout.write(o.decode(\"utf-8\",\"replace\"))")
    :expect "ok"}
   {:cap :py/urllib :lang :python :needs [:py/sys]
    :script (guarded "shutil,socket,threading,urllib.request"
                     "s=socket.socket();s.bind((\"127.0.0.1\",0));s.listen(1)"
                     "def h():"
                     " c=s.accept()[0];c.recv(65536)"
                     " c.sendall(b\"HTTP/1.0 200 OK\\r\\nContent-Length: 2\\r\\n\\r\\nok\");c.close()"
                     "threading.Thread(target=h,daemon=True).start()"
                     (str "r=urllib.request.build_opener(urllib.request.ProxyHandler({}))"
                          ".open(\"http://127.0.0.1:%d/\"%s.getsockname()[1],timeout=5)")
                     "sys.stdout.write(r.read().decode())")
    :expect "ok"}
   {:cap :interp/sh :lang :python :lift false :needs [:py/sys :py/subprocess]
    :script (guarded "subprocess"
                     (str "o=subprocess.run([\"sh\",\"-c\",\"printf %s ok\"],"
                          "stdout=subprocess.PIPE).stdout")
                     "sys.stdout.write(o.decode(\"utf-8\",\"replace\"))")
    :expect "ok"}])

(defn- python-law?
  "True for a python probe whose law is about python itself, so that an sh
  runtime can prove it of the python3 it reaches."
  [probe]
  (not (false? (:lift probe))))

(defn python-via-sh
  "The sh probe for python probe pr: its script run by `python3 -c`, one
  single-quoted word, needing what pr needs plus :interp/python3 and
  :sh/single-quote. The cap is pr's own, so :py/pathlib proved here and
  :py/pathlib proved by a python runtime state one law. Refuses, as
  ex-info, a script no sh word can hold (NUL)."
  [{:keys [cap needs script] :as pr}]
  (let [word (or (lit/sh-single script)
                 (throw (ex-info "a python probe script with NUL cannot be lifted into sh" {:cap cap})))]
    (-> pr
        (dissoc :lang :lift)
        (assoc :needs (into [:interp/python3 :sh/single-quote] (remove #{:interp/python3 :sh/single-quote}) needs)
               :script (str "python3 -c " word)))))

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

(defn py-sh-argv
  "python expression for the argv [sh -c script] as UTF-8 bytes, the argv
  :ctx/python->sh hands to subprocess. Bytes, so no locale can refuse a
  non-ASCII script."
  [script]
  (str "[b\"sh\",b\"-c\",(" (lit/py-double script) ").encode(\"utf-8\")]"))

(defn sh-via-python
  "The python probe for sh probe pr: its script run as
  subprocess.run([\"sh\",\"-c\",script]), the argv :ctx/python->sh runs,
  its stdout copied byte for byte and judged by the same :expect. It needs
  what pr needs plus :py/sys, :py/stdout-buffer, :py/subprocess and
  :interp/sh. The cap is pr's own: in a python runtime :sh/printf names
  the law of the sh that python reaches, as :py/pathlib in an sh runtime
  names the law of the python3 it reaches. Marked :lift false, so it is
  never lifted back into sh."
  [{:keys [needs script] :as pr}]
  (assoc pr
         :lang :python :lift false
         :needs (into [:py/sys :py/stdout-buffer :py/subprocess :interp/sh]
                      (remove #{:py/sys :py/stdout-buffer :py/subprocess :interp/sh}) needs)
         :script (guarded "subprocess"
                          (str "o=subprocess.run(" (py-sh-argv script) ",stdout=subprocess.PIPE).stdout")
                          "sys.stdout.buffer.write(o);sys.stdout.buffer.flush()")))

(defn- sh-law?
  "True for an sh probe whose law is about the sh python reaches, so that
  a python runtime can prove it through subprocess. Not lifted:
  - a cap with a python probe of its own (the :py/ laws, :interp/sh);
  - :bin/<prog>: its contract is that exec finds prog on PATH, which a
    shell running prog does not show (a busybox applet is no file), so a
    python runtime answers it with its own on-demand shutil.which probe;
  - :interp/python3: the python3 an inner sh finds is another interpreter
    than the runtime's, and a profile keeps one verdict per cap."
  [native {:keys [cap]}]
  (not (or (native cap)
           (= "bin" (namespace cap))
           (= :interp/python3 cap))))

(def py-sh-probes
  "The sh probes a python runtime runs through subprocess: every sh probe
  whose law is about the sh it reaches (see sh-law?), lifted by
  sh-via-python, in sh order. They follow py-probes, which proves the :py/
  caps they need.

  This is the probe-side half of a python->sh context layer: a general
  stacked runtime that probes an inner language through a wrapper would
  produce these by running the sh probes through :ctx/python->sh."
  (let [native (into #{} (map :cap) py-probes)]
    (into [] (comp (filter (partial sh-law? native)) (map sh-via-python)) probes)))

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
  "Every built-in probe: the sh probes, then the python ones, then the sh
  probes lifted into python (py-sh-probes). What grammar.core's :core
  module composes. discover! keeps the ones written in the runtime's
  language."
  (-> probes (into py-probes) (into py-sh-probes)))

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
                    builtins and applets all count. In a python runtime it
                    is the sh probe lifted by sh-via-python: the law of the
                    sh that python reaches."
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
      ["sh.cmd" :python] (some-> (on-demand :sh cap) sh-via-python)
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
