(ns grammar.candidate
  "Candidate programs: code plus the provenance that justifies it.

  {:code     program text
   :via      realization and wrapper ids, pre-order
   :requires every capability the text relies on
   :stack    languages from outermost to innermost}")

(defn leaf
  "A single candidate from an encoder result, or none when it returned nil."
  [code]
  (if code [{:code code :via [] :requires #{} :stack []}] []))

(defn- product [seqs]
  (if (empty? seqs)
    [()]
    (let [tail (product (rest seqs))]
      (for [x (first seqs) xs tail] (cons x xs)))))

(defn combine
  "Lazy cartesian product of candidate seqs in argument order, so the walk
  is deterministic. f receives the codes and returns the combined code."
  [f & seqs]
  (for [parts (product seqs)]
    {:code (apply f (map :code parts))
     :via (into [] (mapcat :via) parts)
     :requires (into #{} (mapcat :requires) parts)
     :stack (reduce (fn [a s] (if (> (count s) (count a)) s a)) [] (map :stack parts))}))

(defn stamp
  "Attribute cand to realization r rendered in lang."
  [cand r lang]
  (-> cand
      (update :via #(into [(:id r)] %))
      (update :requires into (:requires r))
      (update :stack #(if (seq %) % [lang]))))
