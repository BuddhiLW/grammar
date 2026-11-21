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
            [grammar.profile :as p]
            [grammar.schema :as s]))

(def ^:private order (juxt :cost :id))

(defn requires-of
  "Capabilities realization r relies on for intent: its static :requires
  plus whatever its optional :requires-of (fn [intent] caps) adds."
  [r intent]
  (into (set (:requires r)) (when-let [f (:requires-of r)] (f intent))))

(defn make-env
  "Planning environment over profile. admit is (fn [profile requires] bool),
  default grammar.profile/admits? (all :yes). profiles maps a wrapper's
  :context key to the profile of the far side of that hop: inside such a
  wrapper the inner layer is planned against it. A context with no
  profile is planned against an empty one, so its needs come back
  :unknown, never guessed from the near side."
  [profile {:keys [realizations wrappers admit profiles] :or {admit p/admits?}}]
  (when-let [dups (seq (keep (fn [[k n]] (when (< 1 n) k)) (frequencies (keep :context wrappers))))]
    (throw (ex-info "two wrappers declare one :context; each hop needs its own profile (grammar.context/chain keys them by path)"
                    {:contexts (vec (sort dups))})))
  {:profile profile
   :profiles (or profiles {})
   :admit-fn admit
   :by-key (update-vals (group-by (juxt :intent :lang) realizations) #(vec (sort-by order %)))
   :wrappers (vec (sort-by order wrappers))
   :admit (fn [reqs] (admit profile reqs))})

(defn at-context
  "env re-rooted at the far side of wrapper w: its profile and admit
  check, when w declares a :context; env itself otherwise."
  [env w]
  (if-let [k (:context w)]
    (let [prof (get (:profiles env) k (p/empty-profile k))
          admit (:admit-fn env p/admits?)]
      (assoc env :profile prof :admit (fn [reqs] (admit prof reqs))))
    env))

(declare candidates)

(defn- render-env [env visited]
  {:lit (fn [lang text] (candidates env visited lang [:lit text]))})

(defn- direct-entries
  "One bound per admitted realization of intent in lang, costing the
  realization's weight until it is rendered."
  [env visited lang [op :as intent]]
  (for [r (get-in env [:by-key [op lang]])
        :let [r (assoc r :requires (requires-of r intent))]
        :when (and (or (= :lit op) (= (:at env) (:target env)))
                   ((:admit env) (:requires r)))]
    {:cost (c/weight r)
     :key [[0 (:cost r 0) (:id r)]]
     :open #(map (fn [cand] (c/stamp cand r lang))
                 ((:render r) (render-env env visited) intent))}))

(defn- merge-far [& fars] (apply merge-with into fars))

(defn- wrap
  "Ascending bounds, one per inner candidate (ascending itself), each
  opening the candidates of wrapper w around it. When w crosses to another
  context, the inner candidate's caps (and w's :inner-requires) were
  checked against that context's profile, so they move under
  :far {context caps} instead of joining the near-side :requires."
  [env visited w inner-cands]
  (let [rank [1 (:cost w 0) (:id w)]
        ctx (:context w)]
    (map (fn [ic]
           (let [cost (+ (:cost w 0) (:cost ic))
                 key (into [rank] (:key ic))
                 [near far] (if ctx
                              [#{} (merge-far (:far ic) {ctx (into (set (:requires ic)) (:inner-requires w))})]
                              [(set (:requires ic)) (:far ic)])]
             {:cost cost
              :key key
              :open #(map (fn [oc]
                            (cond-> {:code (:code oc)
                                     :via (into [(:id w)] (concat (:via ic) (:via oc)))
                                     :requires (into (set (:requires w)) (concat near (:requires oc)))
                                     :stack (into [(:outer w)] (:stack ic))
                                     :cost (+ cost (:cost oc))
                                     :key (into key (:key oc))}
                              (seq far) (assoc :far far)
                              (or ctx (seq (:hops ic))) (assoc :hops (into (if ctx [ctx] []) (:hops ic)))))
                          ((:render w) (render-env env visited) (:code ic)))}))
         inner-cands)))

(defn- usable-here?
  "True when wrapper w may run where env is planning: a context wrapper
  (a relay hop) starts from the context named by its :from (nil: the root
  runtime); a plain wrapper runs anywhere."
  [env w]
  (if (:context w)
    (= (:from w) (:at env))
    true))

(defn- crossing
  "[env' visited'] for the inner layer of w. A context wrapper moves to the
  far side: its profile, a fresh language set, the context remembered so
  no chain of hops revisits it. A plain wrapper adds its inner language."
  [env visited w]
  (if-let [k (:context w)]
    [(assoc (at-context env w) :at k)
     (into #{(:inner w) k} (filter qualified-keyword?) visited)]
    [env (conj visited (:inner w))]))

(defn candidates
  "Lazy, deterministic seq of candidates for intent in lang, cheapest total
  cost first; ties go to a direct realization, then by cost and id. visited
  holds the languages already on the context stack (and the contexts
  already crossed), so delegation never cycles. A wrapper with a :context
  plans its inner layer against that context's profile (see at-context)."
  [env visited lang intent]
  (c/best-first
   (concat
    (direct-entries env visited lang intent)
    (when-not (= :lit (first intent))
      (for [w (:wrappers env)
            :when (and (= lang (:outer w))
                       (usable-here? env w)
                       (not (visited (or (:context w) (:inner w))))
                       ((:admit env) (:requires w))
                       ((:admit (at-context env w)) (:inner-requires w)))]
        {:cost (:cost w 0)
         :key [[1 (:cost w 0) (:id w)]]
         :open #(let [[env' visited'] (crossing env visited w)]
                  (wrap env visited w (candidates env' visited' (:inner w) intent)))})))))

(defn- stack-candidates
  "Candidates for an explicitly requested context stack, outermost first,
  cheapest first."
  [env [lang & more] intent]
  (if-let [inner (first more)]
    (let [w (first (filter #(and (= lang (:outer %)) (= inner (:inner %)) (not (:context %))) (:wrappers env)))]
      (if (and w ((:admit env) (:requires w)))
        (c/best-first [{:cost 0 :key [] :stream (wrap env #{lang} w (stack-candidates env more intent))}])
        ()))
    (c/best-first (direct-entries env #{lang} lang intent))))

;; ---------------------------------------------------------------- text constraints

(defn utf8-len [^String s] (alength (.getBytes s "UTF-8")))

(defn violations
  "Text constraints a program breaks. Empty vector when it satisfies them."
  [{:keys [max-len forbid]} code]
  (let [n (utf8-len code)]
    (cond-> (vec (for [t (sort forbid) :when (str/includes? code t)]
                   {:constraint :forbid :text t}))
      (and max-len (> n max-len)) (conj {:constraint :max-len :limit max-len :actual n}))))

(defn- search [cands text budget]
  (loop [cs (seq cands) n 0 rejected []]
    (cond
      (nil? cs) {:searched n :rejected rejected}
      (>= n budget) {:searched n :rejected rejected :budget-exhausted true}
      :else (let [cand (first cs)
                  vs (violations text (:code cand))]
              (if (empty? vs)
                {:found cand :searched (inc n) :rejected rejected}
                (recur (next cs) (inc n)
                       (cond-> rejected
                         (< (count rejected) 5) (conj {:via (:via cand) :violations vs}))))))))

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

(defn- far-profile [env k] (get (:profiles env) k (p/empty-profile k)))

(defn- far-evidence
  "Evidence ids behind a candidate's far-side caps, context by context."
  [env cand]
  (into [] (mapcat (fn [[k caps]] (p/evidence-for (far-profile env k) caps))) (sort-by key (:far cand))))

(defn- far-needs
  "A candidate's far-side caps still :unknown, as {context [caps]}."
  [env cand]
  (into (sorted-map)
        (keep (fn [[k caps]]
                (when-let [u (seq (filter #(= :unknown (p/verdict (far-profile env k) %)) (sort caps)))]
                  [k (vec u)])))
        (:far cand)))

(defn- relaxed
  "env admitting unknowns everywhere, far sides included: only to say what
  resolving them would unlock."
  [env]
  (assoc env
         :admit #(p/not-refuted? (:profile env) %)
         :admit-fn p/not-refuted?))

(defn solve
  "Plan intent in lang under env. opts: :stack (force a context stack,
  outermost first; overrides lang), :budget (max candidates examined),
  :target (a context key: only a program whose leaf runs on the far side
  of that context is admitted, reached through any chain of hops whose
  :from links up from the root; nil means the root itself).
  Always returns a grammar.schema/Result. A plan through hops carries
  :hops, the contexts crossed in order; a blocked one names far-side
  needs under :needs-at {context [caps]}."
  ([env lang intent] (solve env lang intent {}))
  ([env lang intent {:keys [stack budget] :or {budget 5000} :as opts}]
   (let [env (assoc env :target (:target opts) :at nil)
         profile (:profile env)
         lang (if stack (first stack) lang)]
     (if-not (s/intent? intent)
       {:status :invalid :intent intent :lang lang :profile (:id profile)
        :errors (s/explain-intent intent) :evidence [] :unknowns []}
       (let [text (:text profile)
             text-ev (vec (:evidence text))
             gen (fn [e] (if stack
                           (stack-candidates e stack intent)
                           (candidates e #{lang} lang intent)))
             rows (if stack
                    (explain env (vec (distinct stack)) intent (set (map vec (partition 2 1 stack))))
                    (explain env (reachable-langs env lang) intent nil))
             base {:intent intent :lang lang :profile (:id profile)
                   :unknowns (->> rows (mapcat :unknown) distinct sort vec)}
             {:keys [found searched rejected]} (search (gen env) text budget)]
         (if found
           (assoc base
                  :status :ok
                  :hops (vec (:hops found))
                  :code (:code found)
                  :realization (:via found)
                  :stack (:stack found)
                  :searched searched
                  :constraints {:requires (vec (sort (:requires found)))
                                :text (select-keys text [:max-len :forbid])
                                :bytes (utf8-len (:code found))}
                  :evidence (-> (p/evidence-for profile (:requires found)) (into (far-evidence env found)) (into text-ev)))
           (let [opt (search (gen (relaxed env)) text budget)]
             (if-let [f (:found opt)]
               (cond-> (assoc base
                              :status :blocked-on-unknown
                              :would-select (:via f)
                              :needs (vec (filter #(= :unknown (p/verdict profile %)) (sort (:requires f))))
                              :searched searched
                              :evidence (-> (p/evidence-for profile (:requires f)) (into (far-evidence env f)) (into text-ev))
                              :explain rows)
                 (seq (far-needs env f)) (assoc :needs-at (far-needs env f)))
               (assoc base
                      :status :unsatisfiable
                      :searched searched
                      :rejected rejected
                      :explain rows
                      :evidence (into (p/evidence-for profile (mapcat #(concat (:no %) (:unknown %)) rows))
                                      text-ev))))))))))
