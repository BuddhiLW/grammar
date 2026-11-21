;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.search-order-test
  "solve/candidates against a reference over the real registry, and the
  registry guards of make-env."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [grammar.candidate :as c]
            [grammar.core :as g]
            [grammar.profile :as p]
            [grammar.solve :as solve]))

(defn- product
  "Reference combine: the full product in index-tuple order, unsorted."
  [f & seqs]
  (map #(c/merge-parts f %)
       (reduce (fn [acc s] (for [xs acc x s] (conj xs x))) [[]] seqs)))

(defn dfs
  "Reference enumeration: every candidate for intent in lang, in pre-order
  (direct realizations by [cost id], then wrappers by [cost id], children
  left to right), with no regard to cost."
  [env visited lang [op :as intent]]
  (let [lit (fn [vs] {:lit (fn [l s] (dfs env vs l [:lit s]))})]
    (concat
     (for [r (get-in env [:by-key [op lang]])
           :let [r (assoc r :requires (solve/requires-of r intent))]
           :when ((:admit env) (:requires r))
           cand ((:render r) (lit visited) intent)]
       (c/stamp cand r lang))
     (when-not (= :lit op)
       (for [w (:wrappers env)
             :when (and (= lang (:outer w)) (not (visited (:inner w))) ((:admit env) (:requires w)))
             ic (dfs env (conj visited (:inner w)) (:inner w) intent)
             oc ((:render w) (lit visited) (:code ic))]
         (c/enclose w ic oc))))))

(defn reference
  "What solve/candidates must equal: a stable sort by :cost of the DFS."
  [env lang intent]
  (with-redefs [c/combine product]
    (doall (sort-by :cost (dfs env #{lang} lang intent)))))

(def caps
  "Every capability the composed registry mentions, in a fixed order."
  (let [{:keys [realizations wrappers]} (g/compose g/modules)]
    (vec (sort (distinct (mapcat :requires (concat realizations wrappers)))))))

(def gen-profile
  "A profile declaring a random subset of caps :yes and another :no."
  (gen/fmap (fn [vs]
              (reduce (fn [prof [cap v]] (if v (p/declare-cap prof cap v "gen") prof))
                      (p/empty-profile :gen)
                      (map vector caps vs)))
            (gen/vector (gen/elements [:yes :yes :no nil]) (count caps))))

(def gen-text (gen/elements ["" "a'b" "x y" "$x\n" "\"q\"" "\\"]))

(def gen-intent
  "Small intents: their candidate sets stay a few hundred at most."
  (gen/one-of [(gen/fmap #(vector :str %) gen-text)
               (gen/fmap #(vector :ensure-dir {:path (str "/t/" %)}) gen-text)
               (gen/fmap #(vector :run {:argv ["echo" %]}) gen-text)]))

(def ^:private ccv (juxt :code :via :cost :requires :stack))

(defspec candidates-equal-the-sorted-reference 25
  (prop/for-all [prof gen-profile
                 lang (gen/elements [:sh :python])
                 intent gen-intent
                 relaxed gen/boolean]
    (let [env (cond-> (g/env prof) relaxed (assoc :admit #(p/not-refuted? prof %)))]
      (= (map ccv (reference env lang intent))
         (map ccv (solve/candidates env #{lang} lang intent))))))

(deftest reference-agrees-on-a-large-case
  (let [prof (reduce #(p/declare-cap %1 %2 :yes "t") (p/empty-profile :all) caps)
        env (g/env prof)
        intent [:write-file {:path "/tmp/x y" :content "it's"}]
        want (reference env :sh intent)]
    (is (< 1000 (count want)))
    (is (= (map ccv want) (map ccv (solve/candidates env #{:sh} :sh intent))))))

(defn- refusal [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest make-env-refuses-bad-registries
  (let [prof (p/empty-profile :t)
        ok {:id :x/ok :intent :str :lang :sh :cost 1 :render (constantly [])}]
    (is (nil? (refusal #(solve/make-env prof {:realizations [ok (dissoc ok :cost)]}))) "a missing :cost is 0")
    (testing "a negative cost"
      (is (= {:realizations [{:id :x/neg :cost -1}]}
             (:invalid (refusal #(solve/make-env prof {:realizations [ok (assoc ok :id :x/neg :cost -1)]}))))))
    (testing "a cost that is not a number"
      (is (= {:wrappers [{:id :w/s :cost "5"}]}
             (:invalid (refusal #(solve/make-env prof {:wrappers [{:id :w/s :cost "5" :outer :sh :inner :sh}]}))))))
    (testing "a NaN cost"
      (is (= [:x/nan] (map :id (:realizations (:invalid (refusal #(solve/make-env prof {:realizations [(assoc ok :id :x/nan :cost ##NaN)]}))))))))
    (testing "an id that is not a keyword"
      (is (= {:realizations [{:id "x/str" :cost 1}]}
             (:invalid (refusal #(solve/make-env prof {:realizations [ok (assoc ok :id "x/str")]}))))))
    (testing "the facade refuses through make-env"
      (is (some? (refusal #(g/env prof {:modules [{:id :bad :realizations [(assoc ok :cost -2)]}]})))))))
