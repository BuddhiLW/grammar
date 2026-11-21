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

(defn- keyed
  "Candidates named prefix0.. with the given ascending costs, each keyed by
  its own realization rank."
  [prefix costs]
  (map-indexed (fn [j k] (let [id (keyword (str prefix j))]
                           (cand (name id) k [(c/realization-rank {:id id :cost k})])))
               costs))

(def gen-combine-args
  "[f s1 .. sk] for combine: k ascending, keyed seqs whose codes name their
  position."
  (gen/fmap (fn [cs] (into [vector] (map-indexed (fn [i costs] (keyed (str "s" i "-") costs)) cs)))
            (gen/vector gen-costs 0 3)))

(defn- key-cmp
  "Order on keys: rank by rank, a proper prefix first."
  [a b]
  (or (first (remove zero? (map compare a b)))
      (compare (count a) (count b))))

(defn- rank-order
  "Comparator on candidates: :cost, then :key."
  [a b]
  (let [k (compare (:cost a) (:cost b))]
    (if (zero? k) (key-cmp (:key a) (:key b)) k)))

(defn product-oracle
  "Reference combine: the full product in index-tuple order, stably sorted
  by :cost, then :key."
  [f & seqs]
  (->> (reduce (fn [acc s] (for [xs acc x s] (conj xs x))) [[]] seqs)
       (map #(c/merge-parts f %))
       (sort rank-order)))

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

(defspec combine-is-the-sorted-product 100
  (prop/for-all [[f & seqs] gen-combine-args]
    (= (map (juxt :code :cost :key) (apply product-oracle f seqs))
       (map (juxt :code :cost :key) (apply c/combine f seqs)))))

(deftest combine-is-lazy
  (let [b (ascending "b" [0])]
    (testing "nothing is realized before the first element is asked for"
      (let [n (atom 0)]
        (c/combine str (counting n "a" identity) b)
        (is (= 0 @n))))
    (testing "the k-th output realizes exactly k elements of an input that grows by one per output"
      (doseq [k [1 2 5]]
        (let [n (atom 0)]
          (is (= (map #(str "a" % "b0") (range k))
                 (map :code (take k (c/combine str (counting n "a" identity) b)))))
          (is (= k @n)))))))

;; ---------------------------------------------------------------- best-first

(defn- bound
  "A bound at cost and key opening cands, counting opens in atom opened."
  [cost key opened cands]
  (c/bound cost key #(do (swap! opened inc) cands)))

(def gen-tree
  "Entry trees: [:cand dc], or [:bound dc children] / [:stream dc children],
  dc the cost added over the parent's."
  (gen/recursive-gen
   (fn [inner]
     (gen/tuple (gen/elements [:bound :stream]) (gen/choose 0 3) (gen/vector inner 0 3)))
   (gen/tuple (gen/return :cand) (gen/choose 0 3))))

(defn build
  "The entry for tree node, under a parent costing base and keyed pkey, at
  path: its cost is base plus dc, its key pkey plus its own unique rank, and
  a bound or stream opens its children, sorted ascending."
  [base pkey path [kind dc children]]
  (let [cost (+ base dc)
        id (keyword (apply str "n" (interpose "." path)))
        key (conj pkey (c/realization-rank {:id id :cost cost}))
        kids #(sort rank-order (map-indexed (fn [i ch] (build cost key (conj path i) ch)) children))]
    (case kind
      :cand (cand (name id) cost key)
      :bound (c/bound cost key kids)
      :stream (c/stream cost key (kids)))))

(def gen-entries
  "Queue entries built from 0 to 4 trees: candidates, bounds and streams,
  nested, with unique keys."
  (gen/fmap (fn [trees] (vec (map-indexed (fn [i t] (build 0 [] [i] t)) trees)))
            (gen/vector gen-tree 0 4)))

(defn leaves
  "Every candidate under entries, opening every bound and stream."
  [entries]
  (mapcat (fn [e]
            (case (c/entry-kind e)
              :candidate [e]
              :bound (leaves ((:open e)))
              :stream (leaves (:stream e))))
          entries))

(defspec best-first-is-the-sorted-flattening 200
  (prop/for-all [es gen-entries]
    (= (map (juxt :code :cost) (sort rank-order (leaves es)))
       (map (juxt :code :cost) (c/best-first es)))))

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
   :gen gen-entries
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
      (is (= 3 @n) "s0, s1 and s2, nothing past the last element taken")))
  (testing "an unreached bound behind an infinite cheaper stream is never opened"
    (let [opened (atom 0)
          n (atom 0)]
      (is (= 50 (count (take 50 (c/best-first [{:cost 0 :key [] :stream (counting n "s" (constantly 0))}
                                               (bound 1 [] opened [])])))))
      (is (= 0 @opened))
      (is (= 50 @n)))))

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

;; ---------------------------------------------------------------- attribution

(def ^:private orig-stamp c/stamp)
(def ^:private orig-enclose c/enclose)

(def ^:private realization {:id :r/x :intent :str :lang :sh :cost 3 :requires #{:cap/r}})
(def ^:private literal {:id :r/lit :intent :lit :lang :sh :cost 2 :requires #{}})
(def ^:private wrapper {:id :w/sh->py :outer :sh :inner :python :cost 5 :requires #{:cap/w}})

(deftrifecta stamp
  grammar.candidate/stamp
  {:golden-path "test/golden/candidate/stamp.edn"
   :apply? true
   :cases {:leaf [(first (c/leaf "x")) realization :sh]
           :literal-adds-nothing [(first (c/leaf "x")) literal :sh]
           :keeps-inner-stack [(assoc (cand "y" 1 [[0 1 :inner]]) :requires #{:cap/y} :stack [:perl])
                               realization :sh]}
   :xf #(update % :requires (comp vec sort))
   :mutations [["key-after" (fn [cd r l] (update (orig-stamp cd r l) :key (comp vec reverse)))]
               ["no-weight" (fn [cd r l] (assoc (orig-stamp cd r l) :cost (:cost cd)))]
               ["wrapper-rank" (fn [cd r l] (assoc-in (orig-stamp cd r l) [:key 0 0] 1))]
               ["stack-forced" (fn [cd r l] (assoc (orig-stamp cd r l) :stack [l]))]]})

(deftrifecta enclose
  grammar.candidate/enclose
  {:golden-path "test/golden/candidate/enclose.edn"
   :apply? true
   :cases {:around [wrapper
                    {:code "inner" :via [:p/a] :requires #{:cap/i} :stack [:python] :cost 1 :key [[0 1 :p/a]]}
                    {:code "outer" :via [:s/lit] :requires #{:cap/o} :stack [:sh] :cost 2 :key [[0 1 :s/lit]]}]}
   :xf #(update % :requires (comp vec sort))
   :mutations [["outer-cost-dropped" (fn [w ic oc] (assoc (orig-enclose w ic oc) :cost (+ (:cost w) (:cost ic))))]
               ["via-order" (fn [w ic oc] (update (orig-enclose w ic oc) :via (comp vec reverse)))]
               ["realization-rank" (fn [w ic oc] (assoc-in (orig-enclose w ic oc) [:key 0 0] 0))]
               ["inner-code" (fn [w ic oc] (assoc (orig-enclose w ic oc) :code (:code ic)))]]})

(deftest ranks-and-entries
  (is (= [0 3 :r/x] (c/realization-rank realization)))
  (is (= [1 5 :w/sh->py] (c/wrapper-rank wrapper)))
  (is (= [0 0 :r/none] (c/realization-rank {:id :r/none})) "a missing :cost ranks as 0")
  (is (neg? (compare (c/realization-rank {:id :b :cost 9}) (c/wrapper-rank {:id :a :cost 0})))
      "a realization ranks before a wrapper whatever their costs")
  (is (= [1 0 :w/none] (c/wrapper-rank {:id :w/none})) "a missing :cost ranks as 0")
  (is (= 0 (c/weight {:id :r/none :intent :str})) "a missing :cost weighs 0")
  (is (= 1 (:cost (c/enclose {:id :w/none :outer :sh} (cand "i" 1) (cand "o" 0))))
      "a wrapper without :cost adds 0")
  (let [open (constantly [])]
    (is (= {:cost 1 :key [[0]] :open open} (c/bound 1 [[0]] open))))
  (is (= :bound (c/entry-kind (c/bound 1 [] (constantly [])))))
  (is (= {:cost 0 :key [] :stream [1]} (c/stream [1])))
  (is (= {:cost 2 :key [[0]] :stream nil} (c/stream 2 [[0]] nil))))
