;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.solve
  "Deterministic planner. Pure: a profile and a registry in, a Result out.

  Search is best-first by structural cost: a candidate's cost is the sum
  of its realization and wrapper costs, literal encoders adding 0, so a
  direct realization can lose to a cheaper delegation. Equal costs break
  by pre-order (direct before delegation, realizations by [cost id],
  children left to right). The first candidate whose text satisfies the
  profile's text constraints wins.

  A realization is admitted only when every capability it requires is
  :yes. An :unknown never selects a program. When nothing is admitted the
  search runs again with unknowns let through, only to report what
  resolving them would unlock (:blocked-on-unknown)."
  (:require [clojure.string :as str]
            [grammar.candidate :as c]
            [grammar.law :as law]
            [grammar.profile :as p]
            [grammar.schema :as s]))

(def ^:private order (juxt :cost :id))

(defn requires-of
  "Capabilities realization r relies on for intent: its static :requires
  plus whatever its optional :requires-of (fn [intent] caps) adds."
  [r intent]
  (into (set (:requires r)) (when-let [f (:requires-of r)] (f intent))))

(defn- bad-entries
  "The realizations or wrappers in xs whose :id is not a keyword or whose
  :cost, when given, is not a number >= 0 (NaN is not: it compares false
  both ways and would corrupt the queue order), each as {:id :cost}."
  [xs]
  (vec (for [x xs
             :let [cost (:cost x 0)]
             :when (not (and (keyword? (:id x)) (number? cost) (== cost cost) (not (neg? cost))))]
         (select-keys x [:id :cost]))))

(defn make-env
  "Planning environment over profile. admit is (fn [profile requires] bool),
  default grammar.profile/admits? (all :yes).

  Refuses, as ex-info, a realization or wrapper whose :id is not a keyword
  (ids rank equal costs, and mixed id types do not compare) or whose :cost
  is not a number >= 0 (a cost is a lower bound for everything under it,
  which a negative cost would break)."
  [profile {:keys [realizations wrappers admit] :or {admit p/admits?}}]
  (let [bad (into {} (keep (fn [[k xs]] (when-let [b (seq (bad-entries xs))] [k (vec b)])))
                  {:realizations realizations :wrappers wrappers})]
    (when (seq bad)
      (throw (ex-info "every realization and wrapper needs a keyword :id and a :cost that is a number >= 0"
                      {:invalid bad}))))
  {:profile profile
   :by-key (update-vals (group-by (juxt :intent :lang) realizations) #(vec (sort-by order %)))
   :wrappers (vec (sort-by order wrappers))
   :admit (fn [reqs] (admit profile reqs))})

(declare candidates)

(defn- beneath
  "env for the candidates realization or wrapper x embeds: when x declares
  :shortens? (its output may be shorter than a piece it embeds, breaking
  the length law), :max-len pruning is off for everything under it."
  [env x]
  (cond-> env (:shortens? x) (assoc :prune? false)))

(defn- render-env
  "The env x's :render receives: its (:lit env) renders literals in visited,
  beneath x."
  [env visited x]
  (let [env (beneath env x)]
    {:lit (fn [lang text] (candidates env visited lang [:lit text]))}))


(defn- meter
  "The work account of one search pass: candidates examined (:spent),
  candidates pruned unexamined (:pruned) and the shortest of those
  (:min-bytes). Both kinds of work count against budget."
  [budget]
  (volatile! {:budget budget :spent 0 :pruned 0 :min-bytes nil :cut false}))

(defn- spent?
  "True once meter m's examined plus pruned candidates reach its budget."
  [m]
  (let [{:keys [budget spent pruned]} @m]
    (>= (+ spent pruned) budget)))

(defn- charge-prune!
  "Count one candidate of len bytes pruned in meter m."
  [m len]
  (vswap! m (fn [s] (-> s
                        (update :pruned inc)
                        (update :min-bytes #(if % (min % len) len))))))

(defn- prune-seq
  "cands without those longer than n bytes, each one dropped charged to
  meter m; ends, marking m :cut, once m's budget is spent."
  [m n cands]
  (lazy-seq
   (cond
     (empty? cands) nil
     (some-> m spent?) (do (vswap! m assoc :cut true) nil)
     :else (let [cand (first cands)
                 len (law/utf8-len (:code cand))]
             (if (<= len n)
               (cons cand (prune-seq m n (rest cands)))
               (do (some-> m (charge-prune! len))
                   (prune-seq m n (rest cands))))))))

(defn- within-len
  "cands without those whose code is longer than the profile's :max-len.
  Each one dropped is charged to the env's :meter, and the seq ends once
  the meter's budget is spent (marking it :cut), so pruning can never run
  past the budget. Off when the env's :prune? is false, as it is beneath
  an entry that declares :shortens?.

  Sound by the length law (see grammar.law): no encoder, realization or
  wrapper output is shorter than what it embeds, so every program built
  from a candidate that is too long is too long as well."
  [env cands]
  (let [n (get-in env [:profile :text :max-len])]
    (if (and n (:prune? env true))
      (prune-seq (:meter env) n cands)
      cands)))

(defn- direct-entries
  "One bound per admitted realization of intent in lang, costing the
  realization's weight until it is rendered."
  [env visited lang [op :as intent]]
  (for [r (get-in env [:by-key [op lang]])
        :let [r (assoc r :requires (requires-of r intent))]
        :when ((:admit env) (:requires r))]
    (c/bound (c/weight r)
             [(c/realization-rank r)]
             #(map (fn [cand] (c/stamp cand r lang))
                   ((:render r) (render-env env visited r) intent)))))

(defn- wrap
  "Ascending bounds, one per inner candidate (ascending itself), each
  opening the candidates of wrapper w around it."
  [env visited w inner-cands]
  (map (fn [ic]
         (c/bound (+ (:cost w 0) (:cost ic))
                  (into [(c/wrapper-rank w)] (:key ic))
                  #(map (fn [oc] (c/enclose w ic oc))
                        ((:render w) (render-env env visited w) (:code ic)))))
       inner-cands))

(defn candidates
  "Lazy, deterministic seq of candidates for intent in lang, cheapest total
  cost first; ties go to a direct realization, then by cost and id. visited
  holds the languages already on the context stack, so delegation never
  cycles. Candidates longer than the profile's :max-len are dropped at
  every level (see within-len)."
  [env visited lang intent]
  (within-len
   env
   (c/best-first
    (concat
     (direct-entries env visited lang intent)
     (when-not (= :lit (first intent))
       (for [w (:wrappers env)
             :when (and (= lang (:outer w))
                        (not (visited (:inner w)))
                        ((:admit env) (:requires w)))]
         (c/bound (:cost w 0)
                  [(c/wrapper-rank w)]
                  #(wrap env visited w
                         (candidates (beneath env w) (conj visited (:inner w)) (:inner w) intent)))))))))

(defn- stack-candidates
  "Candidates for an explicitly requested context stack, outermost first,
  cheapest first."
  [env [lang & more] intent]
  (if-let [inner (first more)]
    (let [w (first (filter #(and (= lang (:outer %)) (= inner (:inner %))) (:wrappers env)))]
      (if (and w ((:admit env) (:requires w)))
        (within-len env (c/best-first [(c/stream (wrap env #{lang} w (stack-candidates (beneath env w) more intent)))]))
        ()))
    (within-len env (c/best-first (direct-entries env #{lang} lang intent)))))

;; ---------------------------------------------------------------- text constraints

(def utf8-len
  "Length of a string in UTF-8 bytes (grammar.law/utf8-len)."
  law/utf8-len)

(defn violations
  "Text constraints a program breaks. Empty vector when it satisfies them."
  [{:keys [max-len forbid]} code]
  (let [n (utf8-len code)]
    (cond-> (vec (for [t (sort forbid) :when (str/includes? code t)]
                   {:constraint :forbid :text t}))
      (and max-len (> n max-len)) (conj {:constraint :max-len :limit max-len :actual n}))))

(defn- search
  "Examine cands in order until one satisfies text, counting each examined
  candidate in meter m. Stops with :budget-exhausted once the examined and
  pruned candidates together reach m's budget and something is left. The
  meter is read after the next candidate is realized and before it is
  examined: realizing it may prune, and a prune that spends the budget
  inside a nested stream cuts that stream only (marking m :cut), so the
  candidate realized after it can come from a costlier sibling the budget
  never reached."
  [cands text m]
  (loop [cs cands rejected []]
    (let [s (seq cs)
          n (:spent @m)]
      (cond
        (:cut @m) {:searched n :rejected rejected :budget-exhausted true}
        (nil? s) {:searched n :rejected rejected}
        (spent? m) {:searched n :rejected rejected :budget-exhausted true}
        :else (let [cand (first s)
                    vs (violations text (:code cand))]
                (vswap! m update :spent inc)
                (if (empty? vs)
                  {:found cand :searched (inc n) :rejected rejected}
                  (recur (rest s)
                         (cond-> rejected
                           (< (count rejected) 5) (conj {:via (:via cand) :violations vs})))))))))

(defn- run-pass
  "search over the candidates gen yields under env, metered against budget:
  :pruned counts the candidates the :max-len bound dropped unexamined and
  :pruned-min-bytes is the shortest of them."
  [env gen text budget]
  (let [m (meter budget)
        res (search (gen (assoc env :meter m)) text m)
        {:keys [pruned min-bytes]} @m]
    (cond-> (assoc res :pruned pruned)
      min-bytes (assoc :pruned-min-bytes min-bytes))))

;; ---------------------------------------------------------------- explanation

(defn- reachable-langs [env lang]
  (loop [seen [lang]]
    (let [more (->> (:wrappers env)
                    (filter #(some #{(:outer %)} seen))
                    (map :inner)
                    (remove (set seen))
                    distinct)]
      (if (seq more) (recur (into seen more)) seen))))

(defn- option-row [profile path r]
  (merge {:path path :id (:id r)} (p/classify profile (:requires r))))

(defn explain
  "Every option the planner had for intent, with its required capabilities
  split by verdict. pairs restricts wrappers to those [outer inner] pairs;
  nil means every cross-language wrapper between langs."
  [env langs [op :as intent] pairs]
  (let [profile (:profile env)
        lset (set langs)
        rows-for (fn [o l] (map #(option-row profile [l o] (assoc % :requires (requires-of % intent)))
                                (get-in env [:by-key [o l]])))]
    (vec (concat
          (mapcat #(rows-for op %) langs)
          (mapcat #(rows-for :lit %) langs)
          (for [w (:wrappers env)
                :let [pair [(:outer w) (:inner w)]]
                :when (if pairs
                        (contains? pairs pair)
                        (and (lset (:outer w)) (lset (:inner w)) (not= (:outer w) (:inner w))))]
            (option-row profile [(:outer w) :wrap (:inner w)] w))))))

;; ---------------------------------------------------------------- entry point

(defn- work
  "The work diagnostics of search results passes: :pruned summed, the
  least :pruned-min-bytes, and :budget-exhausted when any pass ran out."
  [passes]
  (let [mins (keep :pruned-min-bytes passes)]
    (cond-> {:pruned (transduce (map :pruned) + 0 passes)}
      (seq mins) (assoc :pruned-min-bytes (reduce min mins))
      (some :budget-exhausted passes) (assoc :budget-exhausted true))))

(defn- not-ok
  "The Result when the strict pass found nothing: :blocked-on-unknown when
  the relaxed pass (unknowns let through) finds a program, else
  :unsatisfiable. plan holds the Result so far (:base), the candidate
  generator (:gen), the explanation rows, the profile's text constraints,
  the budget, and the strict pass's search result (:strict)."
  [env {:keys [base gen rows text budget strict]}]
  (let [profile (:profile env)
        text-ev (vec (:evidence text))
        relaxed (run-pass (assoc env :admit #(p/not-refuted? profile %)) gen text budget)
        base (merge (assoc base :searched (:searched strict) :explain rows)
                    (work [strict relaxed]))]
    (if-let [f (:found relaxed)]
      (assoc base
             :status :blocked-on-unknown
             :would-select (:via f)
             :needs (vec (filter #(= :unknown (p/verdict profile %)) (sort (:requires f))))
             :evidence (into (p/evidence-for profile (:requires f)) text-ev))
      (assoc base
             :status :unsatisfiable
             :rejected (:rejected strict)
             :evidence (into (p/evidence-for profile (mapcat #(concat (:no %) (:unknown %)) rows))
                             text-ev)))))

(defn solve
  "Plan intent in lang under env. opts: :stack (force a context stack,
  outermost first; overrides lang), :budget (max candidates per search
  pass, examined plus pruned, default 5000). Always returns a
  grammar.schema/Result.

  :searched, :rejected, :pruned and :pruned-min-bytes are diagnostics, not
  contract: how many candidates the strict pass examined, the first ones it
  rejected, how many both passes dropped unexamined because their code
  already exceeded :max-len, and the shortest of those. A pass that reaches
  its budget stops and the Result says :budget-exhausted. Pruning changes
  the diagnostics, never the chosen program, as long as no pass reaches
  its budget: past it, a pruned search may stop at a different point than
  an unpruned one would."
  ([env lang intent] (solve env lang intent {}))
  ([env lang intent {:keys [stack budget] :or {budget 5000}}]
   (let [profile (:profile env)
         lang (if stack (first stack) lang)]
     (if-not (s/intent? intent)
       {:status :invalid :intent intent :lang lang :profile (:id profile)
        :errors (s/explain-intent intent) :evidence [] :unknowns []}
       (let [text (:text profile)
             gen (fn [e] (if stack
                           (stack-candidates e stack intent)
                           (candidates e #{lang} lang intent)))
             rows (if stack
                    (explain env (vec (distinct stack)) intent (set (map vec (partition 2 1 stack))))
                    (explain env (reachable-langs env lang) intent nil))
             base {:intent intent :lang lang :profile (:id profile)
                   :unknowns (->> rows (mapcat :unknown) distinct sort vec)}
             {:keys [found searched] :as strict} (run-pass env gen text budget)]
         (if found
           (assoc (merge base (work [strict]))
                  :status :ok
                  :code (:code found)
                  :realization (:via found)
                  :stack (:stack found)
                  :searched searched
                  :constraints {:requires (vec (sort (:requires found)))
                                :text (select-keys text [:max-len :forbid])
                                :bytes (utf8-len (:code found))}
                  :evidence (into (p/evidence-for profile (:requires found)) (vec (:evidence text))))
           (not-ok env {:base base :gen gen :rows rows :text text :budget budget :strict strict})))))))
