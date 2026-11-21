;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.length-law-test
  "The length law that makes :max-len pruning sound: no encoder output is
  shorter than the text it encodes, and no realization or wrapper output is
  shorter than any piece it embeds. So a candidate already over :max-len
  stays over it in every program built from it."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [grammar.candidate :as c]
            [grammar.core :as g]
            [grammar.law :as law]
            [grammar.profile :as p]
            [grammar.solve :as solve]))

(def registry (g/compose g/modules))

(def gen-text
  "Text with quotes, backslashes, newlines, NUL and multi-byte characters."
  (gen/fmap #(apply str %)
            (gen/vector (gen/elements ["a" " " "'" "\"" "\\" "$" "\n" "\u0000" "é" "€" "x"]) 0 12)))

(defn- intent-for
  "A valid intent of op whose text parts are s."
  [op s]
  (case op
    :lit [:lit s]
    :str [:str s]
    :write-file [:write-file {:path (str "/t/" s) :content s}]
    :run [:run {:argv ["echo" s]}]
    :ensure-dir [:ensure-dir {:path (str "/t/" s)}]
    :remove-file [:remove-file {:path (str "/t/" s)}]
    :http-get [:http-get {:url (str "http://h/" (count s))}]))

(defn- piece-env
  "A render env whose every literal is piece, so an output's length can be
  compared with the piece it embeds."
  [piece]
  {:lit (fn [_ _] [{:code piece :via [] :requires #{} :stack [] :cost 0 :key []}])})

(defspec encoders-never-shorten 200
  (prop/for-all [s gen-text]
    (every? (fn [r]
              (every? #(<= (solve/utf8-len s) (solve/utf8-len (:code %)))
                      ((:render r) (piece-env "unused") [:lit s])))
            (filter #(= :lit (:intent %)) (:realizations registry)))))

(defspec realizations-never-shorten-what-they-embed 100
  (prop/for-all [s gen-text piece (gen/fmap #(apply str "P" %) gen-text)]
    (every? (fn [r]
              (every? #(<= (solve/utf8-len piece) (solve/utf8-len (:code %)))
                      ((:render r) (piece-env piece) (intent-for (:intent r) s))))
            (remove #(= :lit (:intent %)) (:realizations registry)))))

(defspec wrappers-never-shorten-what-they-embed 100
  (prop/for-all [code gen-text piece (gen/fmap #(apply str "P" %) gen-text)]
    (every? (fn [w]
              (every? #(<= (solve/utf8-len piece) (solve/utf8-len (:code %)))
                      ((:render w) (piece-env piece) code)))
            (:wrappers registry))))

(deftest every-part-of-the-registry-is-covered
  (is (seq (:wrappers registry)))
  (is (= (set (map :intent (:realizations registry)))
         #{:lit :str :write-file :run :ensure-dir :remove-file :http-get})
      "intent-for knows every intent the registry realizes"))

(defspec the-registry-keeps-the-exported-law 100
  (prop/for-all [s gen-text piece (gen/fmap #(apply str "P" %) gen-text)]
    (empty? (law/length-law-violations registry intent-for s piece))))

(def ^:private shortener
  "A wrapper that drops the code it is given: it breaks the length law."
  {:id :ctx/drop :outer :sh :inner :python :cost 1 :requires #{}
   :render (fn [_ _] (c/leaf "x"))})

(deftest the-law-check-catches-a-shortener-and-honours-the-opt-out
  (is (= [{:id :ctx/drop :code "x"}]
         (law/length-law-violations {:wrappers [shortener]} intent-for "abc" "Pabc")))
  (is (empty? (law/length-law-violations {:wrappers [(assoc shortener :shortens? true)]}
                                         intent-for "abc" "Pabc"))))

(deftest shortens-turns-pruning-off-beneath-it
  (let [sh-lit {:id :t.lit/sh :intent :lit :lang :sh :cost 1 :requires #{}
                :render (fn [_ [_ s]] (c/leaf (str "'" s "'")))}
        py-str {:id :t.str/py :intent :str :lang :python :cost 1 :requires #{}
                :render (fn [_ [_ s]] (c/leaf (str "print('" s "')")))}
        prof (p/declare-text (p/empty-profile :t) :max-len 3 "test")
        plan (fn [w] (solve/solve (solve/make-env prof {:realizations [sh-lit py-str] :wrappers [w]})
                                  :sh [:str "a long text"]))]
    (testing "without the opt-out the long inner program is pruned and nothing fits"
      (is (= :unsatisfiable (:status (plan shortener)))))
    (testing "with :shortens? the inner program survives and the wrapper's short output fits"
      (let [res (plan (assoc shortener :shortens? true))]
        (is (= :ok (:status res)))
        (is (= "x" (:code res)))))))
