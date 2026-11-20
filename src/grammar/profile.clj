(ns grammar.profile
  "Tri-state capability profile: pure construction and queries.

  A capability absent from the profile is :unknown. Nothing in this
  namespace turns :unknown into :yes or :no; only evidence does, and every
  verdict keeps the ids of the evidence that produced it.")

(defn empty-profile [id]
  {:id id :caps {} :text {} :evidence {}})

(defn verdict
  "The verdict for cap: :yes, :no or :unknown (never nil)."
  [profile cap]
  (get-in profile [:caps cap :verdict] :unknown))

(defn- merge-verdict
  "Combine two verdicts about one capability. :no dominates (a refuted law
  or a declared ban wins over a success elsewhere); :yes beats :unknown;
  two unknowns stay unknown."
  [a b]
  (cond (or (= a :no) (= b :no)) :no
        (or (= a :yes) (= b :yes)) :yes
        :else :unknown))

(defn add-evidence
  "Record capability evidence and fold its verdict into the cap entry."
  [profile {:keys [id cap verdict] :as ev}]
  (-> profile
      (assoc-in [:evidence id] ev)
      (update-in [:caps cap]
                 (fn [entry]
                   {:verdict (merge-verdict (:verdict entry :unknown) verdict)
                    :evidence (conj (vec (:evidence entry)) id)}))))

(defn- ev-id [profile tag]
  (keyword (str "ev." (name (:id profile))) tag))

(defn- cap-tag [cap] (str (namespace cap) "." (name cap)))

(defn declare-cap
  "Declare a verdict for cap by policy rather than by probe."
  [profile cap verdict reason]
  (add-evidence profile {:id (ev-id profile (str "declared." (cap-tag cap)))
                         :kind :declared :cap cap :verdict verdict :reason reason}))

(defn declare-text
  "Declare a text constraint on generated programs: :max-len (UTF-8 bytes)
  or :forbid (a set of substrings that must not appear)."
  [profile k v reason]
  (let [id (ev-id profile (str "declared.text." (name k)))]
    (-> profile
        (assoc-in [:evidence id] {:id id :kind :declared :constraint k :reason reason})
        (update :text (fn [t]
                        (-> (case k
                              :max-len (assoc t :max-len v)
                              :forbid (update t :forbid (fnil into #{}) v))
                            (update :evidence (fnil conj []) id)))))))

(defn classify
  "Partition a requires-set by verdict. Each group is sorted."
  [profile requires]
  (let [groups (group-by #(verdict profile %) (sort requires))]
    {:yes (vec (groups :yes)) :no (vec (groups :no)) :unknown (vec (groups :unknown))}))

(defn admits?
  "True only when every required capability is proven :yes."
  [profile requires]
  (every? #(= :yes (verdict profile %)) requires))

(defn not-refuted?
  "True when no required capability is :no. Unknowns pass. Used only to say
  what resolving the unknowns would unlock, never to select a program."
  [profile requires]
  (not-any? #(= :no (verdict profile %)) requires))

(defn evidence-for
  "Evidence ids behind the given capabilities, in capability order."
  [profile caps]
  (into [] (comp (mapcat #(get-in profile [:caps % :evidence])) (distinct)) (sort caps)))
