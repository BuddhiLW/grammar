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
            [grammar.schema :as s])
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

(def ^:private rts
  (delay
    {:py-full (py-rt! :py-full [:sh] {})
     :py-nosub (py-rt! :py-nosub [:sh]
                       {"PYTHONPATH" (site-dir! "import sys\nsys.modules[\"subprocess\"]=None\n")})
     :py-nosh (py-rt! :py-nosh [] {})}))

(def ^:private profs (delay (update-vals @rts g/discover!)))

(def py-caps (mapv :cap probe/py-probes))

(deftest python-runtimes-run-python-probes
  (doseq [[id prof] @profs]
    (testing (str id)
      (is (s/profile? prof))
      (testing "only python probes ran: no sh-only cap was probed"
        (is (= (set py-caps) (set (keys (:caps prof)))))
        (is (not (contains? (:caps prof) :sh/printf))))
      (testing "law 1 + 2: one evidence entry per cap, matching its verdict"
        (doseq [cap py-caps]
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
      (doseq [cap [:py/sys :py/stdout-buffer :py/pathlib :py/urllib :py.os/mkdir :py.os/unlink]]
        (is (= :yes (v py-nosub cap)) (str cap))))
    (testing "no sh on PATH: subprocess imports, but running sh is refuted"
      (is (= :yes (v py-nosh :py/subprocess)))
      (is (= :no (v py-nosh :interp/sh)))
      (is (= "no" (:out (ev py-nosh :py-nosh :interp/sh)))))))

(deftest str-and-write-file-plan-run-and-verify
  (let [out (fx/tmp-dir! "grammar-pyrt-")]
    (doseq [[id runtime] @rts
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
    (testing "full: the wrapper is admitted; only the inner sh's grammar, never probed from python, blocks"
      (let [res (sh-plan py-full)]
        (is (= :blocked-on-unknown (:status res)))
        (is (= :ctx/python->sh (first (:would-select res))))
        (is (seq (:needs res)))
        (is (every? #(= "sh" (namespace %)) (:needs res)) (pr-str (:needs res)))))
    (testing "full: declaring the inner sh's grammar lets python->sh run for real"
      (let [prof (reduce #(p/declare-cap %1 %2 :yes "test: the PATH sh is dash")
                         py-full [:sh/printf :sh/single-quote])
            res (g/realize! (:py-full @rts) prof [:str "via sh's printf"] {:stack [:python :sh]})]
        (is (= :ok (:status res)))
        (is (= :ctx/python->sh (first (:realization res))))
        (is (true? (:ok? (:verified res))) (pr-str (:execution res)))))
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
  (testing "every python probe but :interp/sh is lifted into an sh probe running python3 -c"
    (let [sh-ps (into {} (map (juxt :cap identity)) probe/probes)]
      (doseq [{:keys [cap script needs lift]} probe/py-probes
              :when (not (false? lift))]
        (is (= (str "python3 -c '" script "'") (:script (sh-ps cap))) (str cap))
        (is (every? (set (:needs (sh-ps cap))) (conj needs :interp/python3)) (str cap))))))

(deftest unprobed-languages-are-refused
  (doseq [lang [:php :perl]]
    (let [r (rt/process-runtime {:id :x :lang lang :argv ["/nonexistent"] :env {}})
          e (try (g/discover! r) nil (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (= lang (:lang e)))
      (is (= #{:sh :python} (set (:probe-langs e)))))))
