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

(defn- push
  "Queue entry e (nil is skipped), numbered so equal ranks pop in the order
  they were pushed."
  [st e]
  (if e
    (-> st
        (update :q conj (assoc e :n (:n st)))
        (update :n inc))
    st))

(defn best-first
  "Lazy seq of candidates, cheapest :cost first, from queue entries:

  a candidate        emitted when popped
  {:open f ...}      a bound: f returns an ascending seq of entries
  {:stream s ...}    an ascending seq of entries, opened one at a time

  An entry's :cost is a lower bound for everything it opens; :key ranks
  ties. A bound costs nothing until it reaches the front of the queue."
  [entries]
  (letfn [(step [st]
            (lazy-seq
             (when-let [e (first (:q st))]
               (let [st (update st :q disj e)
                     e (dissoc e :n)]
                 (cond
                   (:open e)
                   (step (push st (-> e (dissoc :open) (assoc :stream ((:open e))))))

                   (contains? e :stream)
                   (if-let [s (seq (:stream e))]
                     (let [h (first s)]
                       (step (-> st
                                 (push h)
                                 (push (assoc e :stream (rest s) :cost (:cost h) :key (:key h))))))
                     (step st))

                   :else (cons e (step st)))))))]
    (step (reduce push {:q (sorted-set-by entry-cmp) :n 0} entries))))

(defn combine
  "Lazy product of candidate seqs, cheapest total cost first. Each seq must
  be ascending by :cost. f receives the codes and returns the combined code.
  Ties pop by key, then by index tuple, so the walk is deterministic."
  [f & seqs]
  (let [vs (vec seqs)
        k (count vs)
        entry (fn [t]
                (let [parts (map-indexed (fn [i j] (nth (vs i) j nil)) t)]
                  (when (every? some? parts)
                    {:code (apply f (map :code parts))
                     :via (into [] (mapcat :via) parts)
                     :requires (into #{} (mapcat :requires) parts)
                     :stack (reduce (fn [a s] (if (> (count s) (count a)) s a)) [] (map :stack parts))
                     :cost (transduce (map :cost) + 0 parts)
                     :key (into [] (mapcat :key) parts)
                     :n t})))
        successors (fn [t]
                     (let [from (or (last (keep-indexed (fn [i j] (when (pos? j) i)) t)) 0)]
                       (keep #(entry (update t % inc)) (range from k))))
        step (fn step [q]
               (lazy-seq
                (when-let [e (first q)]
                  (cons (dissoc e :n)
                        (step (into (disj q e) (successors (:n e))))))))]
    (step (into (sorted-set-by entry-cmp) (keep entry [(vec (repeat k 0))])))))

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
