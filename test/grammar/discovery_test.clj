;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.discovery-test
  "The one discovery test: probe every controlled runtime, then check the
  basic laws that make a profile trustworthy.

  Law 1  every probe leaves exactly one evidence entry, linked from its cap.
  Law 2  the verdict is what the evidence says: :yes printed :expect with an
         exit status, :no ran and printed something else, :unknown never
         ran or was skipped with a reason.
  Law 3  a verdict predicts behaviour: forcing a construct anyway works
         exactly when its verdict is :yes."
  (:require [clojure.test :refer [deftest is testing]]
            [grammar.core :as g]
            [grammar.fixtures :as fx]
            [grammar.probe :as probe]
            [grammar.profile :as p]
            [grammar.schema :as s]))

(def quote-caps
  {:sh.lit/single :sh/single-quote
   :sh.lit/double :sh/double-quote
   :sh.lit/ansi-c :sh/ansi-c-quote})

(defn- forcing
  "admit fn that lets cap through whatever its verdict, everything else
  only when :yes."
  [cap]
  (fn [profile reqs] (every? #(or (= % cap) (= :yes (p/verdict profile %))) reqs)))

(defn- only-quote [profile keep]
  (reduce #(p/declare-cap %1 %2 :no "test: isolate one encoder")
          profile (remove #{keep} (vals quote-caps))))

(deftest discovers-runtimes-and-their-laws
  (let [out (fx/tmp-dir! "grammar-laws-")]
    (doseq [[id runtime] (fx/runtimes!)]
      (let [prof (g/discover! runtime)]
        (testing (str id)
          (is (s/profile? prof))

          (testing "law 1 + 2: evidence matches verdict"
            (doseq [{:keys [cap]} (g/discovery-probes g/modules :sh)]
              (let [ids (get-in prof [:caps cap :evidence])
                    ev (get-in prof [:evidence (first ids)])]
                (testing (str cap)
                  (is (= 1 (count ids)))
                  (is (= (:verdict ev) (p/verdict prof cap)))
                  (case (:verdict ev)
                    :yes (is (and (int? (:exit ev)) (= (:expect ev) (:out ev))))
                    :no (is (and (int? (:exit ev)) (not= (:expect ev) (:out ev))))
                    :unknown (is (or (:error ev) (:reason ev))))))))

          (testing "law 3: quoting verdicts predict literal round trips"
            (doseq [[enc cap] quote-caps
                    :when (not= :unknown (p/verdict prof cap))]
              (let [s "it's \"x\" $HOME\n"
                    res (g/realize! runtime (only-quote prof cap) [:str s] {:admit (forcing cap)})]
                (testing (str enc)
                  (is (= [:sh.str/printf enc] (:realization res)))
                  (is (= (= :yes (p/verdict prof cap)) (true? (:ok? (:verified res))))
                      (pr-str (:execution res)))))))

          (testing "law 3: the redirect verdict predicts a redirecting write"
            (when (not= :unknown (p/verdict prof :sh/redirect))
              (let [path (str out "/" (name id) ".txt")
                    res (g/realize! runtime (only-quote prof :sh/single-quote)
                                    [:write-file {:path path :content "law\n"}]
                                    {:admit (forcing :sh/redirect)})]
                (is (= :sh.write-file/redirect (first (:realization res))))
                (is (= (= :yes (p/verdict prof :sh/redirect)) (true? (:ok? (:verified res))))
                    (pr-str (:execution res)))))))))))
