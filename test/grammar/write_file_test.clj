(ns grammar.write-file-test
  "Acceptance: one :write-file intent, several controlled runtimes, no
  handwritten per-environment command. Planning, rendering, execution and
  verification together."
  (:require [clojure.test :refer [deftest is testing]]
            [grammar.core :as g]
            [grammar.fixtures :as fx]
            [grammar.profile :as p]
            [grammar.schema :as s]))

(def content "it's \"x\" $HOME `id` 100% -n\\\n\tλ 🚀\n")

(defn- evidence-linked?
  "Every evidence id in res resolves in profile. Probe evidence carries the
  exit status it was judged on; declared evidence carries its reason."
  [profile res]
  (every? (fn [id]
            (let [ev (get-in profile [:evidence id])]
              (case (:kind ev)
                :probe (or (int? (:exit ev)) (:error ev) (:reason ev))
                :declared (string? (:reason ev))
                false)))
          (:evidence res)))

(deftest same-intent-across-controlled-runtimes
  (let [rts (fx/runtimes!)
        profs (update-vals rts g/discover!)
        out (fx/tmp-dir! "grammar-wf-")
        write (fn [id prof c]
                (g/realize! (rts id) prof
                            [:write-file {:path (str out "/" (name id) " " (hash prof) ".txt") :content c}]))
        results (into (sorted-map) (for [id (keys rts)] [id (write id (profs id) content)]))
        head (fn [id] (first (:realization (results id))))]

    (testing "every satisfiable runtime writes the exact bytes"
      (doseq [[id res] results :when (not= id :rbash-none)]
        (testing (str id)
          (is (= :ok (:status res)))
          (is (true? (:ok? (:verified res))) (pr-str (:execution res))))))

    (testing "observed restrictions select different realizations"
      (is (= :sh.write-file/redirect (head :bash-full)))
      (is (= :sh.write-file/redirect (head :dash-bare)))
      (is (= :sh.write-file/tee (head :rbash-tee)) "bash -r refuses >, tee is present")
      (is (= [:ctx/sh->python :py.write-file/pathlib] (take 2 (:realization (results :rbash-python))))
          "bash -r refuses >, no tee, python3 present")
      (is (<= 3 (count (distinct (keep (comp first :realization) (vals results)))))))

    (testing "nothing works: unsatisfiable, every refusal traced to a probe run"
      (let [res (results :rbash-none)
            prof (profs :rbash-none)
            rows (into {} (map (juxt :id identity)) (:explain res))]
        (is (= :unsatisfiable (:status res)))
        (is (= [:sh/redirect] (:no (rows :sh.write-file/redirect))))
        (is (= [:bin/tee] (:no (rows :sh.write-file/tee))))
        (is (= [:interp/python3] (:no (rows :ctx/sh->python))))
        (is (= 1 (get-in prof [:evidence :ev.rbash-none/sh.redirect :exit])))
        (is (some #{:py/pathlib} (:unknowns res)) "skipped probe stays an unresolved unknown")))

    (testing "declared policy over discovery: forbidding ' steers to octal printf"
      (let [prof (p/declare-text (profs :dash-bare) :forbid #{"'"} "test: no single quotes")
            res (write :dash-bare prof content)]
        (is (= :ok (:status res)))
        (is (= :sh.write-file/redirect-octal (first (:realization res))))
        (is (not (.contains ^String (:code res) "'")))
        (is (true? (:ok? (:verified res))))))

    (testing "NUL in content: only the octal route can carry it"
      (let [res (write :busybox-bare (profs :busybox-bare) "a\u0000b\n")]
        (is (= :sh.write-file/redirect-octal (first (:realization res))))
        (is (true? (:ok? (:verified res))))))

    (testing "every result carries realization, constraints, linked evidence, unknowns"
      (doseq [[id res] results]
        (testing (str id)
          (is (s/result? res) (s/explain-result res))
          (is (vector? (:unknowns res)))
          (is (seq (:evidence res)))
          (is (evidence-linked? (profs id) res))
          (when (= :ok (:status res))
            (is (seq (:realization res)))
            (is (seq (get-in res [:constraints :requires])))))))))
