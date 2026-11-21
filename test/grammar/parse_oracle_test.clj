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
            [clojure.java.shell]
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

;; ---------------------------------------------------------------- fish and tcsh, in the lab image

(def ^:private lab-image "grammar-relay-lab:1")

(def gen-quoted-text
  "Texts biased to what the fish and csh models accept: single-quoted
  words holding backslash runs, backslash-quote, $, !, newlines and
  non-ASCII, glued with plain letters and blanks."
  (gen/fmap #(apply str %)
            (gen/vector (gen/frequency
                         [[4 (gen/fmap #(str "'" (apply str %) "'")
                                       (gen/vector (gen/elements ["a" "\\\\" "\\'" "\\" "$x" "!" "\n" "λ" " " "%" "\"" "`"]) 0 5))]
                          [3 (gen/elements ["a" "b" " " "  " "-" "=" "%" "λ"])]
                          [1 gen-piece]])
                        1 6)))

(def lab-shells?
  "fish and tcsh run from the docker lab image (GRAMMAR_SSHD=1)."
  (= "1" (System/getenv "GRAMMAR_SSHD")))

(defn- container-argvs
  "For each text, the argv the shell (fish or tcsh) in the lab image gives
  `dump` + text, or nil when it fails. One container per batch: one
  script per line, each line run by the shell from its own file."
  [shell texts]
  (let [scripts (map-indexed (fn [i t] [(str "/tmp/s" i) (str dump t)]) texts)
        mk (apply str (for [[f t] scripts]
                        (str "printf %s " (lit/sh-single t) " > " f "; ")))
        run (apply str (for [[f _] scripts]
                         (str "printf '\\036'; " shell " " f "; printf '\\037%s' $?; ")))
        r (clojure.java.shell/sh "timeout" "120" "docker" "run" "--rm" lab-image "sh" "-c" (str mk run))]
    (for [chunk (rest (str/split (:out r) #"\u001e" -1))]
      (let [[body st] (str/split chunk #"\u001f" 2)]
        (when (= "0" st) (mapv hex->str (str/split body #"\n" -1)))))))

(defn- dialect-agreement
  "[accepted agreed rows] for n generated texts in dialect against shell."
  [dialect shell n]
  (let [ts (concat (gen/sample gen-quoted-text n) (gen/sample gen-shell-text n))
        accepted (filterv #(model-argv dialect %) ts)
        real (container-argvs shell accepted)
        rows (map vector accepted real)]
    [(count accepted) (count (filter (fn [[t r]] (= (conj (model-argv dialect t) "") r)) rows))
     (remove (fn [[t r]] (= (conj (model-argv dialect t) "") r)) rows)]))

(deftest fish-and-tcsh-models-agree-with-the-real-shells
  (if-not lab-shells?
    (println "grammar.parse-oracle-test: fish/tcsh oracles skipped (GRAMMAR_SSHD=1 with" lab-image "built)")
    (doseq [[dialect shell] [[:fish "fish"] [:csh "tcsh"]]]
      (let [[acc agreed bad] (dialect-agreement dialect shell 400)]
        (println "oracle" (name dialect) "accepted" acc "of 400, agreed" agreed)
        (is (> acc 40) "the model must accept enough texts for agreement to mean something")
        (is (= acc agreed) (pr-str (take 3 bad)))))))
