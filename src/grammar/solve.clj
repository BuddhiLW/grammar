;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.solve
  "Deterministic planner. Pure: a profile and a registry in, a Result out.

  Search order is fixed: realizations by [cost id], direct ones before
  delegation through a context wrapper, children left to right. The first
  candidate whose text satisfies the profile's text constraints wins.

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
  default grammar.profile/admits? (all :yes)."
  [profile {:keys [realizations wrappers admit] :or {admit p/admits?}}]
  {:profile profile
   :by-key (update-vals (group-by (juxt :intent :lang) realizations) #(vec (sort-by order %)))
   :wrappers (vec (sort-by order wrappers))
   :admit (fn [reqs] (admit profile reqs))})

(declare candidates)

(defn- render-env [env visited]
  {:lit (fn [lang text] (candidates env visited lang [:lit text]))})

(defn- direct-entries
  "One bound per admitted realization of intent in lang, costing the
  realization's weight until it is rendered."
  [env visited lang [op :as intent]]
  (for [r (get-in env [:by-key [op lang]])
        :let [r (assoc r :requires (requires-of r intent))]
        :when ((:admit env) (:requires r))]
    {:cost (c/weight r)
     :key [[0 (:cost r 0) (:id r)]]
     :open #(map (fn [cand] (c/stamp cand r lang))
                 ((:render r) (render-env env visited) intent))}))

(defn- wrap
  "Ascending bounds, one per inner candidate (ascending itself), each
  opening the candidates of wrapper w around it."
  [env visited w inner-cands]
  (let [rank [1 (:cost w 0) (:id w)]]
    (map (fn [ic]
           (let [cost (+ (:cost w 0) (:cost ic))
                 key (into [rank] (:key ic))]
             {:cost cost
              :key key
              :open #(map (fn [oc]
                            {:code (:code oc)
                             :via (into [(:id w)] (concat (:via ic) (:via oc)))
                             :requires (into (set (:requires w)) (concat (:requires ic) (:requires oc)))
                             :stack (into [(:outer w)] (:stack ic))
                             :cost (+ cost (:cost oc))
                             :key (into key (:key oc))})
                          ((:render w) (render-env env visited) (:code ic)))}))
         inner-cands)))

(defn candidates
  "Lazy, deterministic seq of candidates for intent in lang, cheapest total
  cost first; ties go to a direct realization, then by cost and id. visited
  holds the languages already on the context stack, so delegation never
  cycles."
  [env visited lang intent]
  (c/best-first
   (concat
    (direct-entries env visited lang intent)
    (when-not (= :lit (first intent))
      (for [w (:wrappers env)
            :when (and (= lang (:outer w))
                       (not (visited (:inner w)))
                       ((:admit env) (:requires w)))]
        {:cost (:cost w 0)
         :key [[1 (:cost w 0) (:id w)]]
         :open #(wrap env visited w
                      (candidates env (conj visited (:inner w)) (:inner w) intent))})))))

(defn- stack-candidates
  "Candidates for an explicitly requested context stack, outermost first,
  cheapest first."
  [env [lang & more] intent]
  (if-let [inner (first more)]
    (let [w (first (filter #(and (= lang (:outer %)) (= inner (:inner %))) (:wrappers env)))]
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

(defn solve
  "Plan intent in lang under env. opts: :stack (force a context stack,
  outermost first; overrides lang), :budget (max candidates examined).
  Always returns a grammar.schema/Result."
  ([env lang intent] (solve env lang intent {}))
  ([env lang intent {:keys [stack budget] :or {budget 5000}}]
   (let [profile (:profile env)
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
                  :code (:code found)
                  :realization (:via found)
                  :stack (:stack found)
                  :searched searched
                  :constraints {:requires (vec (sort (:requires found)))
                                :text (select-keys text [:max-len :forbid])
                                :bytes (utf8-len (:code found))}
                  :evidence (into (p/evidence-for profile (:requires found)) text-ev))
           (let [opt (search (gen (assoc env :admit #(p/not-refuted? profile %))) text budget)]
             (if-let [f (:found opt)]
               (assoc base
                      :status :blocked-on-unknown
                      :would-select (:via f)
                      :needs (vec (filter #(= :unknown (p/verdict profile %)) (sort (:requires f))))
                      :searched searched
                      :evidence (into (p/evidence-for profile (:requires f)) text-ev)
                      :explain rows)
               (assoc base
                      :status :unsatisfiable
                      :searched searched
                      :rejected rejected
                      :explain rows
                      :evidence (into (p/evidence-for profile (mapcat #(concat (:no %) (:unknown %)) rows))
                                      text-ev))))))))))
