;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.parse-oracle-test
  "The pure shell model is only a proof if it agrees with real shells.
  Whenever grammar.parse.sh ACCEPTS a text, dash, busybox ash and bash
  must split it into exactly the argv the model says; the model may
  refuse more than a shell accepts, never less."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.string :as str]
            [grammar.fixtures :as fx]
            [grammar.lit :as lit]
            [grammar.parse.sh :as psh]
            [grammar.runtime :as rt]))

(def ^:private shells
  (delay {:dash (fx/sh-runtime! :o-dash :dash [:python3])
          :busybox (fx/sh-runtime! :o-busybox :busybox [:python3])
          :bash (fx/sh-runtime! :o-bash :bash [:python3])}))

(def ^:private dump
  "A command word printing its argv (after itself) as hex of UTF-8, one per line."
  "python3 -c 'import sys,os;[print(os.fsencode(a).hex()) for a in sys.argv[1:]]' ")

(def ^:private hex->str
  (fn [h] (if (= "" h) "" (String. ^bytes (byte-array (map #(unchecked-byte (Integer/parseInt (apply str %) 16)) (partition 2 h))) "UTF-8"))))

(defn- real-argv* [shell text]
  (let [run (rt/execute (@shells shell) (str dump text))]
    (when (= 0 (:exit run))
      (mapv hex->str (str/split (:out run) #"\n" -1)))))

(defn- model-argv [dialect text]
  (when-let [ws (psh/words dialect (str dump text))]
    (vec (drop 3 ws))))

(def gen-piece
  (gen/frequency
   [[5 (gen/elements ["a" "b" " " "  " "'" "\"" "\\" "$" "$'" "\\n" "\\'" "\\\"" "\\\\" "\\$"
                      "\n" "\t" "-" "=" "x=y" "%" "λ" "🚀" "!" "*" "#" "~" "`" "\\\n" "''" "\"\""])]
    [3 (gen/fmap lit/sh-single gen/string-ascii)]
    [2 (gen/fmap lit/sh-double gen/string-ascii)]
    [1 (gen/fmap lit/sh-ansi-c gen/string-ascii)]]))

(def gen-shell-text (gen/fmap #(apply str %) (gen/vector gen-piece 1 8)))

(defn- agrees? [dialect shell text]
  (let [m (model-argv dialect text)]
    (or (nil? m)
        (let [real (real-argv* shell text)]
          (= (conj m "") real)))))

(defspec posix-model-agrees-with-dash 300
  (prop/for-all [t gen-shell-text] (agrees? :posix :dash t)))

(defspec posix-model-agrees-with-busybox 200
  (prop/for-all [t gen-shell-text] (agrees? :posix :busybox t)))

(defspec bash-model-agrees-with-bash 300
  (prop/for-all [t gen-shell-text] (agrees? :bash :bash t)))

(deftest the-model-accepts-enough
  (testing "every encoder's output is accepted and means its input"
    (doseq [s ["" "it's" "a b" "$x `y` \\ \"q\"" "\n!*?" "λ🚀"]]
      (is (= [s] (psh/words (lit/sh-single s))))
      (is (= [s] (psh/words (lit/sh-double s))))
      (is (= [s] (psh/words :bash (lit/sh-ansi-c s))) s))))
