;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

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
                       [:sh/redirect :bin/tee :interp/python3 :interp/php :interp/perl :interp/sh])
        res (g/plan prof :sh wf)
        rows (into {} (map (juxt :id identity)) (:explain res))]
    (is (= :unsatisfiable (:status res)))
    (is (= [:sh/redirect] (:no (rows :sh.write-file/redirect))))
    (is (= [:bin/tee] (:no (rows :sh.write-file/tee))))
    (is (= [:interp/python3] (:no (rows :ctx/sh->python))))
    (is (= [:interp/php] (:no (rows :ctx/sh->php))))
    (is (= [:interp/perl] (:no (rows :ctx/sh->perl))))
    (is (evidence-resolves? prof res))
    (is (some #{:ev.locked/declared.sh.redirect} (:evidence res))))
  (testing "a profile silent on an interpreter is blocked on it, not refuted"
    (let [prof (declared :quiet [:sh/printf :sh/single-quote :sh/pipe :sh/double-quote]
                         [:sh/redirect :bin/tee :interp/python3 :interp/sh])
          res (g/plan prof :sh wf)]
      (is (= :blocked-on-unknown (:status res)))
      (is (every? (set (:unknowns res)) [:interp/php :interp/perl]))
      (is (= [:interp/perl :pl/binmode :pl/open-write :pl/print] (:needs res))
          "sh->perl and sh->php tie at cost 11; the tie goes to the lower id"))))

(deftest text-constraints-steer-the-choice
  (testing "forbidding ' pushes content into octal printf, quoted with \""
    (let [prof (p/declare-text full :forbid #{"'"} "test")
          res (g/plan prof :sh wf)]
      (is (= :ok (:status res)))
      (is (= [:sh.write-file/redirect-octal :sh.lit/double :sh.lit/double] (:realization res)))
      (is (not (.contains ^String (:code res) "'")))
      (is (some #{:ev.full/declared.text.forbid} (:evidence res)))))
  (testing "a length budget nothing fits is unsatisfiable; every candidate is pruned unexamined"
    (let [prof (p/declare-text full :max-len 20 "test")
          res (g/plan prof :sh wf)]
      (is (= :unsatisfiable (:status res)))
      (is (= 0 (:searched res)) "nothing that short exists, so nothing reaches the text check")
      (is (pos? (:pruned res)) "the skipped work is still reported")))
  (testing "pruning keeps the program a length budget admits"
    (let [code (:code (g/plan full :sh wf))
          tight (g/plan (p/declare-text full :max-len (count (.getBytes ^String code "UTF-8")) "test") :sh wf)]
      (is (= :ok (:status tight)))
      (is (= code (:code tight)))
      (is (= (:realization (g/plan full :sh wf)) (:realization tight))))))

(deftest pruned-counts-only-what-max-len-drops
  (is (= 0 (:pruned (g/plan full :sh wf))) "no :max-len, nothing pruned")
  (testing "with both quotes forbidden, the programs that fit 60 bytes are
            searched and rejected; the longer ones are pruned unbuilt"
    (let [prof (-> full (p/declare-text :forbid #{"'" "\""} "test") (p/declare-text :max-len 60 "test"))
          res (g/plan prof :sh wf)]
      (is (= :unsatisfiable (:status res)))
      (is (pos? (:searched res)) "some programs fit and reach the text check")
      (is (< 60 (:pruned-min-bytes res)) "everything pruned was over the limit")
      (is (not (:budget-exhausted res)))
      (is (every? #(every? #{:forbid} (map :constraint (:violations %))) (:rejected res))
          "a rejection is never for length: those are pruned before the check")
      (is (pos? (:pruned res))))))

(defn- within-ms
  "The value of (f) when it returns within ms, else ::timeout."
  [ms f]
  (let [fut (future (f))
        v (deref fut ms ::timeout)]
    (when (= v ::timeout) (future-cancel fut))
    v))

(deftest pruned-candidates-count-against-the-budget
  (let [prof (p/declare-text full :max-len 30 "test")
        words [:run {:argv (into ["echo"] (repeat 10 "abcdefgh"))}]]
    (testing "a product too long at every level stops at the budget, quickly"
      (let [res (within-ms 10000 #(g/plan prof :sh words))]
        (is (map? res) "the search ends; before the fix it pruned 362530 candidates in 20 s")
        (when (map? res)
          (is (= :unsatisfiable (:status res)))
          (is (:budget-exhausted res))
          (is (<= (+ (:searched res) (:pruned res)) (* 2 5000)) "each of the two passes stops at 5000"))))
    (testing "an explicit budget bounds pruned work too"
      (let [res (within-ms 10000 #(g/plan prof :sh words {:budget 1}))]
        (is (map? res))
        (when (map? res)
          (is (:budget-exhausted res))
          (is (<= (+ (:searched res) (:pruned res)) 2)))))
    (testing "the shortest pruned length says why nothing fit"
      (let [res (g/plan prof :sh [:run {:argv ["echo" (apply str (repeat 4 "abcdefgh"))]}])]
        (is (< 30 (:pruned-min-bytes res)))))))

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

(deftest cheapest-nested-stack-wins
  (let [prof (declared :nested
                       [:sh/single-quote :interp/python3 :py/subprocess :interp/php :interp/perl
                        :php.fn/fwrite :php.fn/strlen :pl/binmode :pl/print]
                       [:sh/printf :py/sys :py/stdout-buffer :sh/double-quote :sh/ansi-c-quote])]
    (is (= [:ctx/sh->perl :pl.str/print :pl.lit/single :sh.lit/single]
           (:realization (g/plan prof :sh [:str "hi"])))
        "cheapest total cost, not the first depth-first stack (sh->python->php)")))

(deftest invalid-intent-is-refused
  (is (= :invalid (:status (g/plan full :sh [:http-get {:url "x"}]))))
  (is (= :invalid (:status (g/plan full :sh [:write-file {:path "/x"}])))))
