(ns grammar.roundtrip-test
  "Literal correctness. Random strings go through every literal encoder and
  through random context stacks; real interpreters execute the result and
  must hand back the exact UTF-8 bytes."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [grammar.core :as g]
            [grammar.fixtures :as fx]
            [grammar.lit :as lit]
            [grammar.profile :as p]
            [grammar.runtime :as rt])
  (:import [java.util Arrays]))

;; ---------------------------------------------------------------- generators

(def ^:private gen-cp
  (gen/frequency [[8 (gen/choose 32 126)]
                  [2 (gen/choose 1 31)]
                  [1 (gen/return 127)]
                  [1 (gen/choose 0x80 0x7FF)]
                  [1 (gen/choose 0x800 0xD7FF)]
                  [1 (gen/choose 0xE000 0xFFFD)]
                  [1 (gen/choose 0x10000 0x10FFFF)]]))

(def ^:private gen-nasty
  (gen/elements ["" "'" "''" "'\\''" "\\" "\"" "$" "$HOME" "${x}" "`id`" "$(id)"
                 "%s" "%%" "%" "-n" "--" "!" "\n" "\n\n" "\t" "\\x41" "\\101" "\\'"
                 "*" "~" "#" " " ";" "&&" "|"]))

(def gen-text
  "Strings mixing shell/printf/python metacharacters, controls and astral
  code points. No NUL: no argv word can carry one."
  (gen/fmap #(apply str %)
            (gen/vector (gen/one-of [(gen/fmap (fn [cps] (String. (int-array cps) 0 (count cps)))
                                               (gen/vector gen-cp 1 6))
                                     gen-nasty])
                        0 12)))

(def gen-text-nul
  (gen/fmap (fn [[a b]] (str a "\u0000" b)) (gen/tuple gen-text gen-text)))

;; ---------------------------------------------------------------- runtimes

(def ^:private rts
  (delay {:dash (fx/sh-runtime! :rt-dash :dash [:sh :python3])
          :bash (fx/sh-runtime! :rt-bash :bash [:sh :python3])
          :busybox (fx/sh-runtime! :rt-busybox :busybox [:python3])
          :python (fx/python-runtime! :rt-python [:sh])}))

(def ^:private profiles
  (delay (update-vals (dissoc @rts :python) g/discover!)))

(def quote-caps
  {:sh.lit/single :sh/single-quote
   :sh.lit/double :sh/double-quote
   :sh.lit/ansi-c :sh/ansi-c-quote})

(defn isolate
  "Declare every sh quoting construct except keep :no, so the solver can
  only use one encoder."
  [profile keep]
  (reduce #(p/declare-cap %1 %2 :no "test: isolate one encoder")
          profile
          (remove #{keep} (vals quote-caps))))

;; ---------------------------------------------------------------- sh encoders, via the solver

(defn- encoder-prop [shell encoder]
  (prop/for-all [s gen-text]
    (let [res (g/realize! (@rts shell) (isolate (@profiles shell) (quote-caps encoder)) [:str s])]
      (and (= :ok (:status res))
           (= [:sh.str/printf encoder] (:realization res))
           (true? (:ok? (:verified res)))))))

(defspec sh-single-in-dash 100 (encoder-prop :dash :sh.lit/single))
(defspec sh-single-in-busybox 60 (encoder-prop :busybox :sh.lit/single))
(defspec sh-double-in-dash 100 (encoder-prop :dash :sh.lit/double))
(defspec sh-double-in-busybox 60 (encoder-prop :busybox :sh.lit/double))
(defspec sh-ansi-c-in-bash 100 (encoder-prop :bash :sh.lit/ansi-c))

(defspec printf-octal-carries-nul 100
  (prop/for-all [s gen-text-nul]
    (let [res (g/realize! (@rts :dash) (@profiles :dash) [:str s])]
      (and (= :ok (:status res))
           (= :sh.str/printf-octal (first (:realization res)))
           (true? (:ok? (:verified res)))))))

;; ---------------------------------------------------------------- python encoders, direct

(defn- py-roundtrip? [encode s]
  (let [run (rt/execute (:python @rts) (str "import sys;sys.stdout.buffer.write((" (encode s) ").encode())"))]
    (and (= 0 (:exit run)) (Arrays/equals (.getBytes ^String s "UTF-8") ^bytes (:out-bytes run)))))

(defspec py-single-roundtrip 100 (prop/for-all [s gen-text-nul] (py-roundtrip? lit/py-single s)))
(defspec py-double-roundtrip 100 (prop/for-all [s gen-text-nul] (py-roundtrip? lit/py-double s)))
(defspec py-chr-roundtrip 100 (prop/for-all [s gen-text-nul] (py-roundtrip? lit/py-chr s)))

;; ---------------------------------------------------------------- context stacks

(def gen-stack
  "Outermost is always the shell we launch; inner layers are any mix."
  (gen/fmap #(into [:sh] %) (gen/vector (gen/elements [:sh :python]) 0 3)))

(defspec context-stacks-roundtrip 100
  (prop/for-all [stack gen-stack
                 s gen-text]
    (let [res (g/realize! (@rts :dash) (@profiles :dash) [:str s] {:stack stack})]
      (and (= :ok (:status res))
           (= stack (:stack res))
           (true? (:ok? (:verified res)))))))

(deftest fixed-nasty-strings-through-deep-stacks
  (doseq [stack [[:sh :sh :sh :sh] [:sh :python :sh :python] [:sh :python :python :sh]]
          s ["" "'" "'\\''\"$`\\" "%s%%-n\n\n" "λ🚀\u0001\u007f"]]
    (testing (pr-str stack s)
      (let [res (g/realize! (@rts :dash) (@profiles :dash) [:str s] {:stack stack})]
        (is (= :ok (:status res)))
        (is (= stack (:stack res)))
        (is (true? (:ok? (:verified res))) (pr-str (:code res)))))))
