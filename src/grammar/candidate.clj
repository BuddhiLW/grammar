;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.candidate
  "Candidate programs: code plus the provenance that justifies it.

  {:code     program text
   :via      realization and wrapper ids, pre-order
   :requires every capability the text relies on
   :stack    languages from outermost to innermost}")

(defn leaf
  "A single candidate from an encoder result, or none when it returned nil.
  It costs nothing by itself; stamp adds the realization's cost."
  [code]
  (if code [{:code code :via [] :requires #{} :stack [] :cost 0 :key []}] []))

;; ---------------------------------------------------------------- order

(defn- lex
  "Lexicographic order on keys, vectors of [kind cost id] ranks. A proper
  prefix sorts first, so a bound sorts before every candidate under it."
  [a b]
  (or (first (remove zero? (map compare a b)))
      (compare (count a) (count b))))

(defn- entry-cmp
  "Queue order: total :cost, then :key, then :n, an insertion number or an
  index tuple that makes the order total."
  [a b]
  (let [c (compare (:cost a) (:cost b))]
    (if-not (zero? c)
      c
      (let [k (lex (:key a) (:key b))]
        (if (zero? k) (compare (:n a) (:n b)) k)))))

;; ---------------------------------------------------------------- queue

(def empty-queue
  "A queue holding no entries."
  {:q (sorted-set-by entry-cmp) :n 0})

(defn push
  "queue with entry e added; nil leaves it unchanged. Entries of equal rank
  pop in the order they were pushed."
  [queue e]
  (if e
    (-> queue
        (update :q conj (assoc e :n (:n queue)))
        (update :n inc))
    queue))

(defn pop-min
  "[e rest] for the least entry e of queue and the queue without it, or nil
  when queue is empty. e carries no insertion number."
  [queue]
  (when-let [e (first (:q queue))]
    [(dissoc e :n) (update queue :q disj e)]))

;; ---------------------------------------------------------------- entries

(defn entry-kind
  "The kind of queue entry e: :bound when it has an :open fn, :stream when it
  has a :stream, else :candidate."
  [e]
  (cond
    (:open e) :bound
    (contains? e :stream) :stream
    :else :candidate))

(defmulti expand
  "What popping entry e yields: {:emit candidate} or {:enqueue entries},
  entries to push in order. Only a popped bound calls its :open, and only a
  popped stream realizes its next element."
  entry-kind)

(defmethod expand :candidate [e]
  {:emit e})

(defmethod expand :bound [e]
  {:enqueue [(-> e (dissoc :open) (assoc :stream ((:open e))))]})

(defmethod expand :stream [e]
  (if-let [[h :as s] (seq (:stream e))]
    {:enqueue [h (assoc e :stream (rest s) :cost (:cost h) :key (:key h))]}
    {:enqueue []}))

(defn- drain
  "Lazy seq of the candidates queue yields, popping and expanding one entry
  at a time."
  [queue]
  (lazy-seq
   (when-let [[e queue] (pop-min queue)]
     (let [{:keys [emit enqueue]} (expand e)
           queue (reduce push queue enqueue)]
       (if emit (cons emit (drain queue)) (drain queue))))))

(defn best-first
  "Lazy seq of candidates, cheapest :cost first, from queue entries:

  a candidate        emitted when popped
  {:open f ...}      a bound: f returns an ascending seq of entries
  {:stream s ...}    an ascending seq of entries, opened one at a time

  An entry's :cost is a lower bound for everything it opens; :key ranks
  ties. A bound costs nothing until it reaches the front of the queue."
  [entries]
  (drain (reduce push empty-queue entries)))

;; ---------------------------------------------------------------- product

(defn- longest
  "The longest of stacks, the first among equals; [] when there is none."
  [stacks]
  (reduce (fn [a s] (if (> (count s) (count a)) s a)) [] stacks))

(defn merge-parts
  "One candidate from candidates parts, in order: :code is f applied to
  their codes, :via and :key concatenate, :requires unions, :stack is the
  longest, :cost sums."
  [f parts]
  {:code (apply f (map :code parts))
   :via (into [] (mapcat :via) parts)
   :requires (into #{} (mapcat :requires) parts)
   :stack (longest (map :stack parts))
   :cost (transduce (map :cost) + 0 parts)
   :key (into [] (mapcat :key) parts)})

(defn successors
  "The index tuples after tuple t: t with one coordinate incremented, for
  each coordinate at or after its last nonzero one. From the zero tuple this
  reaches every tuple of naturals exactly once."
  [t]
  (let [from (or (last (keep-indexed (fn [i j] (when (pos? j) i)) t)) 0)]
    (map #(update t % inc) (range from (count t)))))

(defn- tuple-entry
  "The queue entry for index tuple t over the vector of candidate seqs vs,
  numbered by t, or nil when a coordinate runs past its seq."
  [f vs t]
  (let [parts (map-indexed (fn [i j] (nth (vs i) j nil)) t)]
    (when (every? some? parts)
      (assoc (merge-parts f parts) :n t))))

(defn- walk-tuples
  "Lazy seq of the candidates of queue q, a sorted set of tuple entries,
  each popped entry replaced by the entries of its successor tuples."
  [f vs q]
  (lazy-seq
   (when-let [e (first q)]
     (cons (dissoc e :n)
           (walk-tuples f vs (into (disj q e)
                                   (keep #(tuple-entry f vs %))
                                   (successors (:n e))))))))

(defn combine
  "Lazy product of candidate seqs, cheapest total cost first. Each seq must
  be ascending by :cost. f receives the codes and returns the combined code.
  Ties pop by key, then by index tuple, so the walk is deterministic."
  [f & seqs]
  (let [vs (vec seqs)]
    (walk-tuples f vs (into (sorted-set-by entry-cmp)
                            (keep #(tuple-entry f vs %))
                            [(vec (repeat (count vs) 0))]))))

(defn weight
  "What realization r adds to a candidate's :cost: its :cost, except a
  literal encoder, which adds nothing; its cost ranks ties through :key."
  [r]
  (if (= :lit (:intent r)) 0 (:cost r 0)))

(defn stamp
  "Attribute cand to realization r rendered in lang: r's weight is added and
  its rank [0 cost id] leads the key."
  [cand r lang]
  (-> cand
      (update :via #(into [(:id r)] %))
      (update :requires into (:requires r))
      (update :stack #(if (seq %) % [lang]))
      (update :cost + (weight r))
      (update :key #(into [[0 (:cost r 0) (:id r)]] %))))
