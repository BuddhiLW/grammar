;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.prune-test
  ":max-len pruning changes the search diagnostics and nothing else: a
  pruned and an unpruned solve agree on every other key of the Result."
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [grammar.core :as g]
            [grammar.profile :as p]
            [grammar.solve :as solve]))

(def caps
  "Every capability the composed registry mentions, in a fixed order."
  (let [{:keys [realizations wrappers]} (g/compose g/modules)]
    (vec (sort (distinct (mapcat :requires (concat realizations wrappers)))))))

(def gen-verdicts
  "A verdict per cap: mostly :yes, some :no, some left unknown."
  (gen/vector (gen/frequency [[6 (gen/return :yes)] [2 (gen/return :no)] [2 (gen/return nil)]])
              (count caps)))

(def gen-text
  "Short texts with quotes, backslash, $, newline, NUL and multi-byte characters."
  (gen/fmap #(apply str %)
            (gen/vector (gen/elements ["a" "b" " " "'" "\"" "\\" "$" "\n" "\u0000" "é" "€"]) 0 6)))

(def gen-intent
  (gen/let [s gen-text
            op (gen/elements [:str :write-file :run :ensure-dir :remove-file :http-get])]
    (case op
      :str [:str s]
      :write-file [:write-file {:path (str "/t/" s "x") :content s}]
      :run [:run {:argv ["echo" s]}]
      :ensure-dir [:ensure-dir {:path (str "/t/" s "x")}]
      :remove-file [:remove-file {:path (str "/t/" s "x")}]
      :http-get [:http-get {:url (str "http://h/" (count s))}])))

(def gen-forbid
  (gen/elements [#{} #{"'"} #{"\""} #{"'" "\""} #{"\\"} #{"$"}]))

(def gen-stack
  "No stack three times in four, else a short explicit one."
  (gen/frequency [[3 (gen/return nil)]
                  [1 (gen/elements [[:sh :python] [:sh :perl] [:sh :php] [:python :sh] [:sh :sh]])]]))

(def gen-case
  "[profile lang intent opts], the profile's :max-len near the unpruned
  optimum (when there is one) so pruning bites on both sides of it."
  (gen/let [verdicts gen-verdicts
            lang (gen/elements [:sh :python])
            intent gen-intent
            forbid gen-forbid
            stack gen-stack
            delta (gen/choose -12 12)]
    (let [prof (reduce (fn [prof [cap v]] (if v (p/declare-cap prof cap v "gen") prof))
                       (p/empty-profile :gen)
                       (map vector caps verdicts))
          prof (cond-> prof (seq forbid) (p/declare-text :forbid forbid "gen"))
          opts (cond-> {:budget 400} stack (assoc :stack stack))
          free (g/plan prof lang intent opts)
          best (get-in free [:constraints :bytes] 60)]
      [(p/declare-text prof :max-len (max 1 (+ best delta)) "gen") lang intent opts])))

(def ^:private diagnostics [:searched :pruned :pruned-min-bytes :rejected])

(defn- within-ms [ms f]
  (let [fut (future (f))
        v (deref fut ms ::timeout)]
    (when (= v ::timeout) (future-cancel fut))
    v))

(defn- plan-with [prune? prof lang intent opts]
  (solve/solve (assoc (g/env prof) :prune? prune?) lang intent opts))

(defspec pruning-changes-only-the-diagnostics 300
  (prop/for-all [[prof lang intent opts] gen-case]
    (let [pruned (within-ms 20000 #(plan-with true prof lang intent opts))
          free (within-ms 20000 #(plan-with false prof lang intent opts))]
      (or (some #{::timeout} [pruned free])
          (:budget-exhausted free)
          (:budget-exhausted pruned)
          (= (apply dissoc free diagnostics) (apply dissoc pruned diagnostics))))))
