;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.python-runtime-test
  "A runtime whose top-level language is :python (python3 -c), discovered
  with the python probes and planned for in :python. Every restriction is
  real: subprocess is removed by a sitecustomize on PYTHONPATH, sh is
  missing from PATH."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [grammar.core :as g]
            [grammar.fixtures :as fx]
            [grammar.probe :as probe]
            [grammar.profile :as p]
            [grammar.runtime :as rt]
            [grammar.schema :as s]
            [grammar.lit :as lit]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]])
  (:import [java.io File]))

(defn- site-dir!
  "Directory holding a sitecustomize.py with the given python source."
  [src]
  (let [d (fx/tmp-dir! "grammar-site-")]
    (spit (File. ^String d "sitecustomize.py") src)
    d))

(defn- py-rt!
  "python3 -c runtime, PATH = bin dir of tool-ks, extra env merged in."
  [id tool-ks extra-env]
  (rt/process-runtime {:id id :lang :python :argv [(fx/tools :python3) "-c"]
                       :env (merge {"PATH" (fx/bin-dir! tool-ks) "LC_ALL" "C.UTF-8"} extra-env)}))

(defn audit-site
  "sitecustomize source installing an audit hook that raises PermissionError
  on every event in events (a prefix match) and, when writes? is true, on
  every open whose flags ask to write. The e2e lab restricts python this
  way; the import machinery tolerates an OSError, so the interpreter still
  starts."
  [events writes?]
  (str "import os,sys\n"
       "_B=(" (str/join "" (map #(str "\"" % "\",") events)) ")\n"
       "_W=os.O_WRONLY|os.O_RDWR|os.O_CREAT|os.O_APPEND|os.O_TRUNC\n"
       "def _h(ev,a):\n"
       " if ev.startswith(_B) or (" (if writes? "True" "False")
       " and ev==\"open\" and len(a)>2 and isinstance(a[2],int) and a[2]&_W):\n"
       "  raise PermissionError(\"audit: \"+ev)\n"
       "sys.addaudithook(_h)\n"))

(def lab-events
  "What the e2e lab's hook refuses: spawning and the network."
  ["subprocess.Popen" "os.system" "os.exec" "os.posix_spawn" "os.spawn" "os.fork"
   "socket." "urllib.Request"])

(defn- audited-rt!
  [id events writes?]
  (py-rt! id [:sh] {"PYTHONPATH" (site-dir! (audit-site events writes?))
                    "PYTHONDONTWRITEBYTECODE" "1"}))

(def ^:private rts
  (delay
    {:py-full (py-rt! :py-full [:sh] {})
     :py-nosub (py-rt! :py-nosub [:sh]
                       {"PYTHONPATH" (site-dir! "import sys\nsys.modules[\"subprocess\"]=None\n")})
     :py-nosh (py-rt! :py-nosh [] {})
     :py-audit (audited-rt! :py-audit lab-events true)
     :py-audit-fs (audited-rt! :py-audit-fs ["os.mkdir" "os.remove" "os.symlink"] false)}))

(def ^:private profs (delay (update-vals @rts g/discover!)))

(def py-caps (mapv :cap probe/py-probes))

(def py-sh-caps
  "The sh caps a python runtime probes through subprocess."
  (mapv :cap probe/py-sh-probes))

(deftest python-runtimes-run-python-probes
  (doseq [[id prof] @profs]
    (testing (str id)
      (is (s/profile? prof))
      (testing "only python probes ran: the python laws and the sh ones lifted through subprocess"
        (is (= (set (concat py-caps py-sh-caps)) (set (keys (:caps prof)))))
        (is (not (contains? (:caps prof) :interp/php)) "php and perl probe only through sh"))
      (testing "law 1 + 2: one evidence entry per cap, matching its verdict"
        (doseq [cap (concat py-caps py-sh-caps)]
          (let [ids (get-in prof [:caps cap :evidence])
                ev (get-in prof [:evidence (first ids)])]
            (testing (str cap)
              (is (= 1 (count ids)))
              (is (some? ev) "evidence id resolves in the profile")
              (is (= (probe/evidence-id id cap) (first ids)))
              (is (= (:verdict ev) (p/verdict prof cap)))
              (case (:verdict ev)
                :yes (is (and (int? (:exit ev)) (= (:expect ev) (:out ev))))
                :no (is (and (int? (:exit ev)) (not= (:expect ev) (:out ev))))
                :unknown (is (or (:error ev) (:reason ev)))))))))))

(deftest verdicts-match-each-restriction
  (let [{:keys [py-full py-nosub py-nosh]} @profs
        v p/verdict
        ev (fn [prof id cap] (get-in prof [:evidence (probe/evidence-id id cap)]))]
    (testing "full python: every python cap holds, sh is reachable"
      (doseq [cap py-caps]
        (is (= :yes (v py-full cap)) (str cap (pr-str (ev py-full :py-full cap))))))
    (testing "subprocess removed: :py/subprocess refuted, :interp/sh skipped as unknown"
      (is (= :no (v py-nosub :py/subprocess)))
      (is (= :unknown (v py-nosub :interp/sh)))
      (is (str/includes? (str (:reason (ev py-nosub :py-nosub :interp/sh)))
                         "prerequisite not proven: :py/subprocess=no"))
      (doseq [cap [:py/sys :py/stdout-buffer :py/pathlib :py/open-write :py/urllib :py.os/mkdir :py.os/unlink]]
        (is (= :yes (v py-nosub cap)) (str cap))))
    (testing "no sh on PATH: subprocess imports, but running sh is refuted"
      (is (= :yes (v py-nosh :py/subprocess)))
      (is (= :no (v py-nosh :interp/sh)))
      (is (= "no" (:out (ev py-nosh :py-nosh :interp/sh)))))))

(deftest audit-hooks-are-detected
  (let [{:keys [py-audit py-audit-fs]} @profs
        v p/verdict
        ev (fn [prof id cap] (get-in prof [:evidence (probe/evidence-id id cap)]))]
    (testing "the lab's hook: spawning, sockets and write-mode open are refuted, with the refusal kept"
      (doseq [[cap event] [[:py/subprocess "subprocess.Popen"] [:py/urllib "socket"] [:py/open-write "open"]]]
        (is (= :no (v py-audit cap)) (str cap))
        (is (str/includes? (str (:err (ev py-audit :py-audit cap))) (str "audit: " event)) (str cap)))
      (is (= :unknown (v py-audit :interp/sh)) "sh is never tried once subprocess is refuted")
      (doseq [cap [:py/sys :py/stdout-buffer :py/pathlib :py.os/mkdir :py.os/unlink]]
        (is (= :yes (v py-audit cap)) (str cap))))
    (testing "a hook on os.mkdir and os.remove: the fs caps are refuted, everything else holds"
      (is (= :no (v py-audit-fs :py.os/mkdir)))
      (is (= :no (v py-audit-fs :py.os/unlink)))
      (doseq [cap [:py/open-write :py/subprocess :py/urllib :interp/sh]]
        (is (= :yes (v py-audit-fs cap)) (str cap))))))

(deftest audited-plans-refuse-or-reroute
  (let [{:keys [py-audit py-audit-fs]} @profs
        dir (fx/tmp-dir! "grammar-audit-")]
    (testing ":str still runs: stdout needs no forbidden operation"
      (let [res (g/realize! (:py-audit @rts) py-audit [:str "it's \u00e9"])]
        (is (= :ok (:status res)))
        (is (true? (:ok? (:verified res))) (pr-str (:execution res)))))
    (testing "write-file, http-get, run and delegation to sh are refused, citing the refutation"
      (doseq [[intent opts cap] [[[:write-file {:path (str dir "/w") :content "x"}] {} :py/open-write]
                                 [[:http-get {:url "http://127.0.0.1:1/"}] {} :py/urllib]
                                 [[:run {:argv ["true"]}] {} :py/subprocess]
                                 [[:str "x"] {:stack [:python :sh]} :py/subprocess]]]
        (let [res (g/plan py-audit :python intent opts)]
          (is (= :unsatisfiable (:status res)) (pr-str intent (:needs res)))
          (is (some #{(probe/evidence-id :py-audit cap)} (:evidence res)) (pr-str intent)))))
    (testing "fs ops are refused where os.mkdir/os.remove are hooked, nothing runs"
      (doseq [intent [[:ensure-dir {:path (str dir "/d")}] [:remove-file {:path (str dir "/f")}]]]
        (let [res (g/realize! (:py-audit-fs @rts) py-audit-fs intent)]
          (is (not= :ok (:status res)) (pr-str intent))
          (is (nil? (:execution res))))))))

(deftest str-and-write-file-plan-run-and-verify
  (let [out (fx/tmp-dir! "grammar-pyrt-")]
    (doseq [[id runtime] (select-keys @rts [:py-full :py-nosub :py-nosh])
            :let [prof (@profs id)]]
      (testing (str id)
        (doseq [s ["" "it's \"x\" $HOME `id` \\" "\u00fcn\u00ef\ncode\ttab" "nul\u0000byte"]]
          (let [res (g/realize! runtime prof [:str s])]
            (is (= :ok (:status res)) (pr-str s res))
            (is (= [:python] (:stack res)))
            (is (= :py.str/stdout (first (:realization res))))
            (is (every? #(contains? (:evidence prof) %) (:evidence res)))
            (is (true? (:ok? (:verified res))) (pr-str s (:execution res)))))
        (let [path (str out "/" (name id) " it's.txt")
              res (g/realize! runtime prof [:write-file {:path path :content "law's\n\u00e9"}])]
          (is (= :ok (:status res)) (pr-str res))
          (is (= :py.write-file/pathlib (first (:realization res))))
          (is (every? #(contains? (:evidence prof) %) (:evidence res)))
          (is (true? (:ok? (:verified res))) (pr-str (:execution res))))))))

(deftest delegation-to-sh-follows-the-evidence
  (let [{:keys [py-full py-nosub py-nosh]} @profs
        sh-plan #(g/plan % :python [:str "x"] {:stack [:python :sh]})]
    (testing "full: python probed the grammar of the sh it reaches, so python->sh plans and runs"
      (let [res (g/realize! (:py-full @rts) py-full [:str "via sh's printf: it's \u00e9"] {:stack [:python :sh]})]
        (is (= :ok (:status res)) (pr-str (:needs res)))
        (is (= [:ctx/python->sh :sh.str/printf :sh.lit/single] (take 3 (:realization res))))
        (is (= [:python :sh] (:stack res)))
        (is (every? #(contains? (:evidence py-full) %) (:evidence res)))
        (is (some #{(probe/evidence-id :py-full :sh/single-quote)} (:evidence res)))
        (is (true? (:ok? (:verified res))) (pr-str (:execution res)))))
    (testing "full: the lifted sh verdicts are dash's, judged by the sh probes' own expect"
      (is (= :yes (p/verdict py-full :sh/printf)))
      (is (= :no (p/verdict py-full :sh/ansi-c-quote)) "dash has no $'...'")
      (is (not (contains? (:caps py-full) :interp/python3))
          "the python3 an inner sh finds is another interpreter: not lifted")
      (is (not-any? #(= "bin" (namespace %)) (keys (:caps py-full)))
          ":bin/ caps are answered on demand by shutil.which, never by a shell"))
    (testing "the lifted sh probes are skipped where subprocess or sh is refuted"
      (doseq [[prof id why] [[py-nosub :py-nosub ":py/subprocess=no"] [py-nosh :py-nosh ":interp/sh=no"]]
              cap py-sh-caps]
        (is (= :unknown (p/verdict prof cap)) (str id cap))
        (is (str/includes? (str (get-in prof [:evidence (probe/evidence-id id cap) :reason])) why) (str id cap))))
    (testing "subprocess refuted: python->sh is unsatisfiable, the refutation is cited"
      (let [res (sh-plan py-nosub)]
        (is (= :unsatisfiable (:status res)))
        (is (some #{(probe/evidence-id :py-nosub :py/subprocess)} (:evidence res)))))
    (testing "sh refuted: python->sh is unsatisfiable, the refutation is cited"
      (let [res (sh-plan py-nosh)]
        (is (= :unsatisfiable (:status res)))
        (is (some #{(probe/evidence-id :py-nosh :interp/sh)} (:evidence res)))))
    (testing "a :run of sh is refused once exec is shown not to find it"
      (let [res (g/plan-resolving! (:py-nosh @rts) py-nosh [:run {:argv ["sh" "-c" "true"]}])]
        (is (not= :ok (:status (:result res))))
        (is (= :no (p/verdict (:profile res) :bin/sh)))))))

(deftest shared-cap-names
  (testing "an sh runtime proves the python laws under the same names"
    (let [sh-prof (g/discover! (fx/sh-runtime! :py-shared :dash [:sh :python3]))
          py-prof (:py-full @profs)]
      (doseq [cap (remove #{:interp/sh} py-caps)]
        (is (= :yes (p/verdict sh-prof cap) (p/verdict py-prof cap)) (str cap)))))
  (testing "a python runtime proves the sh laws of the sh it reaches under the same names"
    (let [sh-prof (g/discover! (fx/sh-runtime! :sh-shared :dash [:sh]))
          py-prof (:py-full @profs)]
      (doseq [cap py-sh-caps]
        (is (= (p/verdict sh-prof cap) (p/verdict py-prof cap)) (str cap)))))
  (testing "every lifted sh probe runs the sh script verbatim through subprocess, same expect"
    (let [sh-ps (into {} (map (juxt :cap identity)) probe/probes)]
      (doseq [{:keys [cap script expect needs]} probe/py-sh-probes]
        (is (str/includes? script (probe/py-sh-argv (:script (sh-ps cap)))) (str cap))
        (is (= (:expect (sh-ps cap)) expect) (str cap))
        (is (every? (set needs) (concat [:py/subprocess :interp/sh] (:needs (sh-ps cap)))) (str cap)))))
  (testing "every python probe but :interp/sh is lifted into an sh probe running python3 -c"
    (let [sh-ps (into {} (map (juxt :cap identity)) probe/probes)]
      (doseq [{:keys [cap script needs lift]} probe/py-probes
              :when (not (false? lift))]
        (is (= (str "python3 -c " (lit/sh-single script)) (:script (sh-ps cap))) (str cap))
        (is (every? (set (:needs (sh-ps cap))) (conj needs :interp/python3)) (str cap))))))

(def ^:private orig-sh-via-python probe/sh-via-python)

(def ^:private gen-sh-probe
  "An sh probe: printable ASCII script (quotes, backslashes, $ included), needs from a small pool."
  (gen/let [script (gen/fmap str/join (gen/vector (gen/fmap char (gen/choose 32 126)) 0 20))
            needs (gen/vector-distinct (gen/elements [:sh/printf :sh/single-quote :interp/sh :sh/pipe]))]
    {:cap :sh/x :needs needs :script script :expect "ok"}))

(deftrifecta sh-via-python
  grammar.probe/sh-via-python
  {:golden-path "test/golden/probe/sh-via-python.edn"
   :cases {:printf (first probe/probes)
           :quotes {:cap :sh/q :needs [:sh/printf] :script "printf %s \"o\"'k' \\ $HOME" :expect "ok"}
           :overlap {:cap :sh/o :needs [:interp/sh :sh/printf] :script "sh -c 'printf %s ok'" :expect "ok"}}
   :gen gen-sh-probe
   :pred (fn [{:keys [lang lift needs script expect]}]
           (and (= :python lang) (false? lift) (= "ok" expect)
                (= [:py/sys :py/stdout-buffer :py/subprocess :interp/sh] (take 4 needs))
                (= (count needs) (count (distinct needs)))
                (str/includes? script "[b\"sh\",b\"-c\",")))
   :mutations [["keeps-sh-lang" (fn [pr] (dissoc (orig-sh-via-python pr) :lang))]
               ["liftable" (fn [pr] (dissoc (orig-sh-via-python pr) :lift))]
               ["drops-inner-needs" (fn [pr] (assoc (orig-sh-via-python pr) :needs [:py/sys :py/stdout-buffer :py/subprocess :interp/sh]))]
               ["no-interp-sh" (fn [pr] (update (orig-sh-via-python pr) :needs #(vec (remove #{:interp/sh} %))))]
               ["unquoted" (fn [pr] (update (orig-sh-via-python pr) :script str/replace "\\\"" "\""))]]})

(defn- busybox-sh-rt!
  "python runtime whose PATH holds only sh, a link to busybox: tee and
  printf are applets the shell runs, not files exec finds."
  []
  (let [d (fx/tmp-dir! "grammar-bbsh-")]
    (java.nio.file.Files/createSymbolicLink (.toPath (File. ^String d "sh")) (.toPath (File. ^String (fx/tools :busybox)))
                                            (make-array java.nio.file.attribute.FileAttribute 0))
    (rt/process-runtime {:id :py-bbsh :lang :python :argv [(fx/tools :python3) "-c"]
                         :env {"PATH" d "LC_ALL" "C.UTF-8"}})))

(deftest bin-caps-follow-exec-not-the-shell
  (let [r (busybox-sh-rt!)
        prof (g/discover! r)
        {res :result after :profile} (g/plan-resolving! r prof [:run {:argv ["tee" "/dev/null"]}])]
    (is (= :yes (p/verdict prof :sh/pipe)) "busybox sh runs its tee applet in a pipe")
    (is (= :unknown (p/verdict prof :bin/tee)) "but no shell probe answers for exec")
    (is (= :no (p/verdict after :bin/tee)) "shutil.which finds no tee file on PATH")
    (is (not= :ok (:status res)) "so :run of tee is never planned")))

(def ^:private ascii-env {"LC_ALL" "C" "PYTHONUTF8" "0" "PYTHONCOERCECLOCALE" "0"})

(deftest non-ascii-argv-under-an-ascii-locale
  (let [r (rt/process-runtime {:id :py-ascii :lang :python :argv [(fx/tools :python3) "-c"]
                               :env (assoc ascii-env "PATH" (fx/bin-dir! [:sh]))})
        prof (g/resolve! r (g/discover! r) [:bin/sh])
        dir (fx/tmp-dir! "grammar-ascii-")]
    (is (= "ascii" (:out (rt/execute r "import sys;sys.stdout.write(sys.getfilesystemencoding())"))))
    (doseq [[intent opts] [[[:str "caf\u00e9 \u03bb"] {:stack [:python :sh]}]
                           [[:run {:argv ["sh" "-c" "test \"$1\" = caf\u00e9" "x" "caf\u00e9"]}] {}]
                           [[:write-file {:path (str dir "/caf\u00e9") :content "\u00e9"}] {}]
                           [[:ensure-dir {:path (str dir "/d\u00e9")}] {}]
                           [[:remove-file {:path (str dir "/caf\u00e9")}] {}]]]
      (let [res (g/realize! r prof intent opts)]
        (is (= :ok (:status res)) (pr-str intent))
        (is (true? (:ok? (:verified res))) (pr-str intent (:execution res)))))))

(defspec py-sh-argv-evaluates-to-the-script 100
  (prop/for-all [s (gen/such-that #(not (str/includes? % "\u0000")) gen/string 50)]
    (let [run (rt/execute (:py-full @rts)
                          (str "import sys;sys.stdout.buffer.write(b'\\0'.join(" (probe/py-sh-argv s) "))"))]
      (java.util.Arrays/equals (.getBytes (str "sh\u0000-c\u0000" s) "UTF-8") ^bytes (:out-bytes run)))))

(deftest unprobed-languages-are-refused
  (doseq [lang [:php :perl]]
    (let [r (rt/process-runtime {:id :x :lang lang :argv ["/nonexistent"] :env {}})
          e (try (g/discover! r) nil (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (= lang (:lang e)))
      (is (= #{:sh :python} (set (:probe-langs e)))))))
