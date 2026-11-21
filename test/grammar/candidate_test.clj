;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.candidate-test
  "Best-first merge and lazy product of candidates: order, coverage, laziness."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [grammar.candidate :as c]
            [hive-test.trifecta :refer [deftrifecta]]))

(defn cand
  "A bare candidate with code, cost and key."
  ([code cost] (cand code cost []))
  ([code cost key] {:code code :via [code] :requires #{code} :stack [] :cost cost :key key}))

(defn ascending
  "Candidates named prefix0.. with the given ascending costs."
  [prefix costs]
  (map-indexed (fn [j k] (cand (str prefix j) k)) costs))

(defn ascending?
  "True when xs is ordered by non-decreasing :cost."
  [xs]
  (let [costs (map :cost xs)]
    (every? true? (map <= costs (rest costs)))))

(defn counting
  "An unchunked infinite seq of candidates, cost (cost-of i), counting
  realized elements in atom n."
  [n prefix cost-of]
  (map #(do (swap! n inc) (cand (str prefix %) (cost-of %))) (iterate inc 0)))

(def gen-costs
  "An ascending vector of small costs."
  (gen/fmap #(vec (sort %)) (gen/vector (gen/choose 0 5) 0 4)))

(def gen-combine-args
  "[f s1 .. sk] for combine: k ascending seqs whose codes name their position."
  (gen/fmap (fn [cs] (into [vector] (map-indexed (fn [i costs] (ascending (str "s" i "-") costs)) cs)))
            (gen/vector gen-costs 0 3)))

(def ^:private orig-merge-parts c/merge-parts)
(def ^:private orig-combine c/combine)
(def ^:private orig-best-first c/best-first)

;; ---------------------------------------------------------------- successors

(deftrifecta successors
  grammar.candidate/successors
  {:golden-path "test/golden/candidate/successors.edn"
   :cases {:zero [0 0 0] :first [1 0 0] :middle [0 2 0] :last [0 0 3] :single [4] :empty []}
   :xf vec
   :gen (gen/vector gen/nat 0 4)
   :pred #(every? vector? %)
   :mutations [["every-coordinate" (fn [t] (map #(update t % inc) (range (count t))))]
               ["after-last-nonzero"
                (fn [t] (let [from (or (last (keep-indexed (fn [i j] (when (pos? j) i)) t)) -1)]
                          (map #(update t % inc) (range (inc from) (count t)))))]
               ["none" (fn [_] ())]]})

(defn reachable
  "Every tuple reachable from the zero tuple of width k with coordinates
  below n, with multiplicity."
  [k n]
  (loop [frontier [(vec (repeat k 0))] seen []]
    (if-let [t (first frontier)]
      (recur (into (vec (rest frontier)) (filter #(every? (fn [j] (< j n)) %)) (c/successors t))
             (conj seen t))
      seen)))

(defspec successors-reach-every-tuple-once 50
  (prop/for-all [k (gen/choose 1 3) n (gen/choose 1 4)]
    (let [ts (reachable k n)]
      (and (= (count ts) (count (distinct ts)))
           (= (count ts) (long (Math/pow n k)))))))

;; ---------------------------------------------------------------- merge-parts

(deftrifecta merge-parts
  grammar.candidate/merge-parts
  {:golden-path "test/golden/candidate/merge-parts.edn"
   :apply? true
   :cases {:two [str [{:code "a" :via [:x] :requires #{:r1} :stack [:sh] :cost 1 :key [[0 1 :x]]}
                      {:code "b" :via [:y :z] :requires #{:r1 :r2} :stack [:sh :perl] :cost 2 :key [[0 2 :y]]}]]
           :tie-stack [vector [{:code 1 :via [] :requires #{} :stack [:a] :cost 0 :key []}
                               {:code 2 :via [] :requires #{} :stack [:b] :cost 0 :key []}]]
           :none [str []]}
   :xf #(update % :requires (comp vec sort))
   :mutations [["stack-last" (fn [f parts] (assoc (orig-merge-parts f parts) :stack (:stack (last parts) [])))]
               ["no-cost" (fn [f parts] (assoc (orig-merge-parts f parts) :cost 0))]
               ["reversed-via" (fn [f parts] (update (orig-merge-parts f parts) :via (comp vec reverse)))]
               ["key-of-first" (fn [f parts] (assoc (orig-merge-parts f parts) :key (vec (:key (first parts)))))]]})

;; ---------------------------------------------------------------- combine

(deftrifecta combine
  grammar.candidate/combine
  {:golden-path "test/golden/candidate/combine.edn"
   :apply? true
   :cases {:pair [str (ascending "a" [0 1 3]) (ascending "b" [0 2])]
           :triple [vector (ascending "a" [0 1]) (ascending "b" [1 1]) (ascending "c" [0 4])]
           :ties [str (ascending "a" [1 1]) (ascending "b" [1 1])]
           :empty-arg [str (ascending "a" [0 1]) ()]
           :zero-arg [vector]}
   :xf #(mapv (juxt :code :cost) %)
   :gen gen-combine-args
   :pred ascending?
   :mutations [["reversed" (fn [f & s] (reverse (apply orig-combine f s)))]
               ["first-only" (fn [f & s] (take 1 (apply orig-combine f s)))]
               ["dropped" (fn [f & s] (rest (apply orig-combine f s)))]
               ["doubled" (fn [f & s] (mapcat #(vector % %) (apply orig-combine f s)))]]})

(defspec combine-yields-every-tuple-once 100
  (prop/for-all [[f & seqs] gen-combine-args]
    (let [out (apply c/combine f seqs)
          codes (map :code out)]
      (and (ascending? out)
           (= (count codes) (count (distinct codes)))
           (= (count codes) (reduce * 1 (map count seqs)))))))

(deftest combine-is-lazy
  (let [n (atom 0)]
    (is (= ["a0b0" "a1b0"] (map :code (take 2 (c/combine str (counting n "a" identity) (ascending "b" [0]))))))
    (is (< @n 5) "an infinite ascending seq is realized only as far as needed")))

;; ---------------------------------------------------------------- best-first

(defn- bound
  "A bound at cost and key opening cands, counting opens in atom opened."
  [cost key opened cands]
  {:cost cost :key key :open #(do (swap! opened inc) cands)})

(deftrifecta best-first
  grammar.candidate/best-first
  {:golden-path "test/golden/candidate/best-first.edn"
   :cases {:plain [(cand "c" 3) (cand "a" 1) (cand "b" 2)]
           :key-ties [(cand "y" 1 [[1 0 :y]]) (cand "x" 1 [[0 0 :x]])]
           :push-order [(cand "first" 1) (cand "second" 1)]
           :mixed [(cand "direct" 4 [[0 4 :d]])
                   {:cost 1 :key [[1 1 :w]] :open (constantly [(cand "w1" 2 [[1 1 :w] [0 1 :p]])
                                                                (cand "w2" 5 [[1 1 :w] [0 4 :q]])])}
                   {:cost 0 :key [] :stream [(cand "s0" 0) (cand "s3" 3) (cand "s6" 6)]}]
           :nested [{:cost 0 :key [] :open (constantly [{:cost 1 :key [] :open (constantly [(cand "deep" 2)])}
                                                        (cand "shallow" 3)])}]
           :empty-stream [{:cost 0 :key [] :stream []} (cand "z" 1)]
           :empty []}
   :xf #(mapv (juxt :code :cost) %)
   :gen (gen/fmap (fn [costs] (shuffle (map-indexed #(cand (str "c" %1) %2) costs)))
                  (gen/vector (gen/choose 0 9) 0 8))
   :pred ascending?
   :mutations [["reversed" (fn [es] (reverse (orig-best-first es)))]
               ["candidates-only" (fn [es] (sort-by :cost (remove #(or (:open %) (contains? % :stream)) es)))]
               ["stream-head-only" (fn [es] (orig-best-first (map #(if (contains? % :stream) (update % :stream (partial take 1)) %) es)))]
               ["dropped" (fn [es] (rest (orig-best-first es)))]]})

(deftest best-first-is-lazy
  (testing "a bound is opened only when it reaches the queue front"
    (let [opened (atom 0)
          out (c/best-first [(cand "cheap" 1) (bound 5 [] opened [(cand "dear" 6)])])]
      (is (= "cheap" (:code (first out))))
      (is (= 0 @opened))
      (is (= ["cheap" "dear"] (map :code out)))
      (is (= 1 @opened))))
  (testing "a stream is realized one element at a time"
    (let [n (atom 0)
          out (c/best-first [{:cost 0 :key [] :stream (counting n "s" #(* 2 %))} (cand "odd" 3)])]
      (is (= ["s0" "s1" "odd" "s2"] (map :code (take 4 out))))
      (is (< @n 6))))
  (testing "an unreached bound behind an infinite cheaper stream is never opened"
    (let [opened (atom 0)
          n (atom 0)]
      (is (= 50 (count (take 50 (c/best-first [{:cost 0 :key [] :stream (counting n "s" (constantly 0))}
                                               (bound 1 [] opened [])])))))
      (is (= 0 @opened)))))

;; ---------------------------------------------------------------- queue and entries

(defn- drain-queue
  "The entries of queue in pop order."
  [queue]
  (when-let [[e more] (c/pop-min queue)]
    (cons e (drain-queue more))))

(deftest queue-pops-in-rank-order
  (let [q (reduce c/push c/empty-queue [(cand "b" 2) nil (cand "a" 1 [[1]]) (cand "a0" 1 [[0]]) (cand "a1" 1 [[1]])])]
    (is (= ["a0" "a" "a1" "b"] (map :code (drain-queue q))) "cost, then key, then push order")
    (is (nil? (c/pop-min c/empty-queue)))
    (is (not-any? #(contains? % :n) (drain-queue q)))))

(deftest entry-kinds-expand
  (is (= :candidate (c/entry-kind (cand "a" 1))))
  (is (= :bound (c/entry-kind {:cost 0 :open (constantly [])})))
  (is (= :stream (c/entry-kind {:cost 0 :stream nil})))
  (is (= {:emit (cand "a" 1)} (c/expand (cand "a" 1))))
  (is (= {:enqueue []} (c/expand {:cost 0 :key [] :stream []})))
  (let [[h more] (:enqueue (c/expand {:cost 0 :key [] :stream [(cand "h" 2 [[7]])]}))]
    (is (= (cand "h" 2 [[7]]) h))
    (is (= [2 [[7]] nil] [(:cost more) (:key more) (seq (:stream more))])))
  (let [[s] (:enqueue (c/expand {:cost 1 :key [[1]] :open (constantly [(cand "x" 1)])}))]
    (is (= :stream (c/entry-kind s)))
    (is (= [(cand "x" 1)] (:stream s)))
    (is (not (contains? s :open)))))
