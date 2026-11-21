;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.candidate
  "Candidate programs: code plus the provenance that justifies it.

  {:code     program text
   :via      realization and wrapper ids, pre-order
   :requires every capability the text relies on
   :stack    languages from outermost to innermost
   :cost     structural cost, a number >= 0: realization and wrapper costs
             summed, a literal encoder adding 0
   :key      the ranks on the way in pre-order, each [kind cost id], kind
             0 for a realization and 1 for a wrapper; it orders equal costs}

  Every key is mandatory: stamp, enclose and merge-parts add to :cost and
  :key, so a hand-built candidate without them throws. Ranks come from
  realization-rank and wrapper-rank and queue entries from bound and
  stream, so the [kind cost id] grammar of keys is written only here.")

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
  "queue with entry e added, numbered n, by default the queue's insertion
  count; nil leaves it unchanged. Entries of equal :cost and :key pop by
  number, so in push order when numbered by default.

  That FIFO is the queue's, not best-first's: a popped bound re-enters as a
  stream with a fresh number. Pre-order among equal costs in best-first
  rests on keys being unique, not on push order."
  ([queue e] (push queue e (:n queue)))
  ([queue e n]
   (if e
     (-> queue
         (update :q conj (assoc e :n n))
         (update :n inc))
     queue)))

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
  carrying t as :t, or nil when a coordinate runs past its seq."
  [f vs t]
  (let [parts (map-indexed (fn [i j] (nth (vs i) j nil)) t)]
    (when (every? some? parts)
      (assoc (merge-parts f parts) :t t))))

(defn- walk-tuples
  "Lazy seq of the candidates of queue, a queue of tuple entries. popped,
  the entry emitted just before, is replaced by the entries of its successor
  tuples only when the next element is asked for, so no input seq is
  realized ahead of the output."
  [f vs queue popped]
  (lazy-seq
   (let [queue (reduce (fn [q t] (push q (tuple-entry f vs t) t))
                       queue
                       (when popped (successors (:t popped))))]
     (when-let [[e queue] (pop-min queue)]
       (cons (dissoc e :t) (walk-tuples f vs queue e))))))

(defn best-first-product
  "Lazy product of the candidate seqs in seqs, cheapest total cost first.
  Each seq must be ascending by :cost. f receives the codes and returns the
  combined code. Ties pop by key, then by index tuple, so the walk is
  deterministic. An element of an input seq is realized only once an
  output needs it."
  [f seqs]
  (let [vs (vec seqs)
        zero (vec (repeat (count vs) 0))]
    (lazy-seq (walk-tuples f vs (push empty-queue (tuple-entry f vs zero) zero) nil))))

(def ^:dynamic *product*
  "How combine builds a product: (fn [f seqs] candidates). Default
  best-first-product. The seam a reference test binds to the full,
  unordered product, to check the search against an enumeration that does
  not share its ordering code."
  best-first-product)

(defn combine
  "Lazy product of candidate seqs, cheapest total cost first (see
  best-first-product, the default *product*)."
  [f & seqs]
  (*product* f seqs))

(defn weight
  "What realization r adds to a candidate's :cost: its :cost, except a
  literal encoder, which adds nothing; its cost ranks ties through :key."
  [r]
  (if (= :lit (:intent r)) 0 (:cost r 0)))

(defn realization-rank
  "The key rank of realization r: [0 cost id]."
  [r]
  [0 (:cost r 0) (:id r)])

(defn wrapper-rank
  "The key rank of wrapper w: [1 cost id], after every realization of equal
  cost, so a direct realization wins a tie against a delegation."
  [w]
  [1 (:cost w 0) (:id w)])

(defn bound
  "A queue entry costing cost, ranked by key, that opens (open) only when it
  reaches the front: open returns an ascending seq of entries, each costing
  at least cost and keyed under key."
  [cost key open]
  {:cost cost :key key :open open})

(defn stream
  "A queue entry over s, an ascending seq of entries, realized one element
  at a time. cost and key rank it until its head is realized."
  ([s] (stream 0 [] s))
  ([cost key s] {:cost cost :key key :stream s}))

(defn stamp
  "Attribute cand to realization r rendered in lang: r's weight is added and
  its rank leads the key."
  [cand r lang]
  (-> cand
      (update :via #(into [(:id r)] %))
      (update :requires into (:requires r))
      (update :stack #(if (seq %) % [lang]))
      (update :cost + (weight r))
      (update :key #(into [(realization-rank r)] %))))

(defn enclose
  "The candidate of wrapper w around inner candidate ic, oc being the
  candidate w rendered for ic's code in w's outer language: oc's code,
  provenance in pre-order (w, then ic, then oc), w's cost added to both,
  w's rank leading the key."
  [w ic oc]
  {:code (:code oc)
   :via (into [(:id w)] (concat (:via ic) (:via oc)))
   :requires (into (set (:requires w)) (concat (:requires ic) (:requires oc)))
   :stack (into [(:outer w)] (:stack ic))
   :cost (+ (:cost w 0) (:cost ic) (:cost oc))
   :key (into [(wrapper-rank w)] (concat (:key ic) (:key oc)))})
