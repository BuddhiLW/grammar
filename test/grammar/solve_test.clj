(ns grammar.solve-test
  "Pure planner behaviour over declared profiles: no processes."
  (:require [clojure.test :refer [deftest is testing]]
            [grammar.core :as g]
            [grammar.profile :as p]
            [grammar.schema :as s]))

(defn declared
  "Profile whose verdicts are all declarations: yes caps, no caps, rest unknown."
  [id yes no]
  (as-> (p/empty-profile id) prof
    (reduce #(p/declare-cap %1 %2 :yes "test") prof yes)
    (reduce #(p/declare-cap %1 %2 :no "test") prof no)))

(def wf [:write-file {:path "/tmp/x y" :content "it's $x\n"}])

(def full
  (declared :full [:sh/printf :sh/single-quote :sh/double-quote :sh/redirect :sh/pipe
                   :bin/tee :interp/python3 :py/pathlib] []))

(defn- evidence-resolves? [profile res]
  (every? #(contains? (:evidence profile) %) (:evidence res)))

(deftest deterministic
  (is (= (g/plan full :sh wf) (g/plan full :sh wf)))
  (is (= "printf %s 'it'\\''s $x\n' >'/tmp/x y'" (:code (g/plan full :sh wf)))))

(deftest unknown-stays-unknown
  (let [prof (declared :half [:sh/printf :sh/single-quote] [])
        res (g/plan prof :sh wf)]
    (is (= :blocked-on-unknown (:status res)))
    (is (nil? (:code res)) "an unknown never selects a program")
    (is (= [:sh/redirect] (:needs res)) "names exactly what would unlock the cheapest option")
    (is (= [:sh.write-file/redirect :sh.lit/single :sh.lit/single] (:would-select res)))
    (is (some #{:sh/redirect} (:unknowns res)))
    (is (= :unknown (p/verdict prof :sh/redirect)) "planning does not invent a verdict")
    (is (not (contains? (:caps prof) :sh/redirect)))))

(deftest refuted-capabilities-are-explained
  (let [prof (declared :locked [:sh/printf :sh/single-quote :sh/pipe :sh/double-quote]
                       [:sh/redirect :bin/tee :interp/python3 :interp/sh])
        res (g/plan prof :sh wf)
        rows (into {} (map (juxt :id identity)) (:explain res))]
    (is (= :unsatisfiable (:status res)))
    (is (= [:sh/redirect] (:no (rows :sh.write-file/redirect))))
    (is (= [:bin/tee] (:no (rows :sh.write-file/tee))))
    (is (= [:interp/python3] (:no (rows :ctx/sh->python))))
    (is (evidence-resolves? prof res))
    (is (some #{:ev.locked/declared.sh.redirect} (:evidence res)))))

(deftest text-constraints-steer-the-choice
  (testing "forbidding ' pushes content into octal printf, quoted with \""
    (let [prof (p/declare-text full :forbid #{"'"} "test")
          res (g/plan prof :sh wf)]
      (is (= :ok (:status res)))
      (is (= [:sh.write-file/redirect-octal :sh.lit/double :sh.lit/double] (:realization res)))
      (is (not (.contains ^String (:code res) "'")))
      (is (some #{:ev.full/declared.text.forbid} (:evidence res)))))
  (testing "a length budget nothing fits is unsatisfiable, with the rejections shown"
    (let [prof (p/declare-text full :max-len 20 "test")
          res (g/plan prof :sh wf)]
      (is (= :unsatisfiable (:status res)))
      (is (= :max-len (-> res :rejected first :violations first :constraint))))))

(deftest every-result-carries-its-justification
  (doseq [prof [full
                (declared :half [:sh/printf :sh/single-quote] [])
                (declared :locked [:sh/printf :sh/single-quote] [:sh/redirect :bin/tee :interp/python3])]
          intent [wf [:str "a'b"]]]
    (let [res (g/plan prof :sh intent)]
      (testing [(:id prof) intent (:status res)]
        (is (s/result? res) (s/explain-result res))
        (is (vector? (:unknowns res)))
        (is (evidence-resolves? prof res))
        (when (= :ok (:status res))
          (is (seq (:realization res)))
          (is (seq (get-in res [:constraints :requires])))
          (is (seq (:evidence res))))))))

(deftest invalid-intent-is-refused
  (is (= :invalid (:status (g/plan full :sh [:http-get {:url "x"}]))))
  (is (= :invalid (:status (g/plan full :sh [:write-file {:path "/x"}])))))
