;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.parse.sh
  "Pure model of how a shell turns program text into argv: tokenizing,
  quote removal and pipelines, for the literal subset the planner emits.

  The model is an oracle, so it is strict: whatever it does not model as
  literal text (parameter, command and arithmetic expansion, globs, tilde,
  comments, redirections, lists, reserved words, assignments) is REFUSED,
  never guessed. A refusal means \"this text is not proven to reach argv
  unchanged in this dialect\".

  Dialects:
    :posix  POSIX sh (dash, busybox ash): '..' \"..\" and backslash
    :bash   :posix plus ANSI-C $'..' quoting
    :fish   single quotes take \\\\ and \\' escapes; double quotes and
            every unquoted backslash refused
    :csh    single quotes may not hold a newline, ! is history even
            inside them; double quotes and every unquoted backslash
            refused

  Unquoted ~ is refused in every dialect: bash expands it after = or : in
  any assignment-shaped word, even outside command position.

  parse returns {:pipeline [[word ...] ...]} or {:error msg :at index}.")

(defn- refuse [msg i]
  (throw (ex-info msg {::refuse true :at i})))

(def reserved
  "Words that change meaning in command position."
  #{"if" "then" "else" "elif" "fi" "do" "done" "case" "esac" "while" "until"
    "for" "in" "{" "}" "!" "function" "select" "[[" "]]" "time" "coproc"})

(def ^:private name-char? #(or (Character/isLetterOrDigit (char %)) (= \_ %)))

(defn- single-quoted
  "Body of a single-quoted string opened before index i: [text next-i]."
  [dialect ^String s i]
  (loop [j i acc (StringBuilder.)]
    (when (>= j (count s)) (refuse "unterminated single quote" i))
    (let [c (.charAt s j)]
      (cond
        (= c \') [(str acc) (inc j)]
        (and (= dialect :fish) (= c \\) (< (inc j) (count s))
             (#{\\ \'} (.charAt s (inc j))))
        (recur (+ j 2) (.append acc (.charAt s (inc j))))
        (and (= dialect :csh) (= c \newline)) (refuse "csh: newline in single quotes" j)
        (and (= dialect :csh) (= c \!)) (refuse "csh: history expansion" j)
        :else (recur (inc j) (.append acc c))))))

(defn- double-quoted
  "Body of a POSIX double-quoted string opened before index i."
  [^String s i]
  (loop [j i acc (StringBuilder.)]
    (when (>= j (count s)) (refuse "unterminated double quote" i))
    (let [c (.charAt s j)]
      (cond
        (= c \") [(str acc) (inc j)]
        (#{\$ \`} c) (refuse "expansion inside double quotes" j)
        (= c \\) (let [n (when (< (inc j) (count s)) (.charAt s (inc j)))]
                   (cond (nil? n) (refuse "unterminated double quote" i)
                         (#{\$ \` \" \\} n) (recur (+ j 2) (.append acc n))
                         (= n \newline) (recur (+ j 2) acc)
                         :else (recur (+ j 2) (-> acc (.append \\) (.append n)))))
        :else (recur (inc j) (.append acc c))))))

(def ^:private ansi-simple
  {\a 7 \b 8 \e 27 \E 27 \f 12 \n 10 \r 13 \t 9 \v 11 \\ 92 \' 39 \" 34 \? 63})

(defn- octal-digit? [c] (and c (<= (int \0) (int c) (int \7))))

(defn- ansi-c-quoted
  "Body of a bash $'..' string opened before index i. Octal escapes give
  one char each (bytes < 128 only; higher octal is refused: in bash it
  names a raw byte, not a code point)."
  [^String s i]
  (loop [j i acc (StringBuilder.)]
    (when (>= j (count s)) (refuse "unterminated $'" i))
    (let [c (.charAt s j)
          at (fn [k] (when (< k (count s)) (.charAt s k)))]
      (cond
        (= c \') [(str acc) (inc j)]
        (= c \\)
        (let [n (at (inc j))]
          (cond
            (octal-digit? n)
            (let [ds (take-while octal-digit? (map at (range (inc j) (min (+ j 4) (count s)))))
                  v (Integer/parseInt (apply str ds) 8)]
              (when (> v 127) (refuse "$': octal escape above 127" j))
              (when (zero? v) (refuse "$': NUL ends the word" j))
              (recur (+ j 1 (count ds)) (.append acc (char v))))
            (ansi-simple n) (recur (+ j 2) (.append acc (char (ansi-simple n))))
            :else (refuse "$': escape not modeled" j)))
        :else (recur (inc j) (.append acc c))))))

(defn- step-word
  "Parse one word starting at i (not blank). [word next-i]."
  [dialect ^String s i first-word?]
  (loop [j i acc (StringBuilder.) pure? true]
    (let [c (when (< j (count s)) (.charAt s j))]
      (cond
        (or (nil? c) (#{\space \tab \|} c)) [(str acc) j]
        (= c \newline) (refuse "newline separates commands" j)
        (#{\; \& \< \> \( \) \`} c) (refuse (str "operator or expansion " c) j)
        (#{\* \? \[ \{ \} \!} c) (refuse (str "unquoted " c) j)
        (and (#{\# \~} c) (= j i)) (refuse (str "unquoted " c " at word start") j)
        (and (= c \=) pure? first-word? (pos? (.length acc))) (refuse "assignment in command position" j)
        (= c \$) (if (and (= dialect :bash) (< (inc j) (count s)) (= \' (.charAt s (inc j))))
                   (let [[t k] (ansi-c-quoted s (+ j 2))] (recur k (.append acc ^String t) false))
                   (refuse "unquoted $" j))
        (= c \\) (let [n (when (< (inc j) (count s)) (.charAt s (inc j)))]
                   (cond (nil? n) (refuse "trailing backslash" j)
                         (#{:fish :csh} dialect) (refuse (str (name dialect) ": unquoted backslash not modeled") j)
                         (= n \newline) (recur (+ j 2) acc pure?)
                         :else (recur (+ j 2) (.append acc n) false)))
        (= c \~) (refuse "unquoted ~ (bash expands it after = or : in any word that looks like an assignment)" j)
        (= c \') (let [[t k] (single-quoted dialect s (inc j))] (recur k (.append acc ^String t) false))
        (= c \") (if (#{:posix :bash} dialect)
                   (let [[t k] (double-quoted s (inc j))] (recur k (.append acc ^String t) false))
                   (refuse (str (name dialect) ": double quotes not modeled") j))
        :else (recur (inc j) (.append acc c) (and pure? (name-char? c)))))))

(defn- parse* [dialect ^String s]
  (loop [i 0 words [] cmds []]
    (let [c (when (< i (count s)) (.charAt s i))
          close (fn [ws]
                  (when (empty? ws) (refuse "empty command" i))
                  (when (reserved (first ws)) (refuse (str "reserved word " (first ws)) i))
                  (conj cmds ws))]
      (cond
        (nil? c) {:pipeline (close words)}
        (#{\space \tab} c) (recur (inc i) words cmds)
        (and (= c \\) (< (inc i) (count s)) (= \newline (.charAt s (inc i))))
        (if (#{:fish :csh} dialect)
          (refuse (str (name dialect) ": backslash-newline not modeled") i)
          (recur (+ i 2) words cmds))
        (= c \|) (if (and (< (inc i) (count s)) (= \| (.charAt s (inc i))))
                   (refuse "|| list" i)
                   (recur (inc i) [] (close words)))
        :else (let [[w j] (step-word dialect s i (empty? words))]
                (recur j (conj words w) cmds))))))

(defn parse
  "Parse program text s in dialect (default :posix): {:pipeline [argv ...]}
  or {:error msg :at i} when s uses anything the model does not prove literal."
  ([s] (parse :posix s))
  ([dialect s]
   (try (parse* dialect s)
        (catch clojure.lang.ExceptionInfo e
          (if (::refuse (ex-data e))
            {:error (ex-message e) :at (:at (ex-data e))}
            (throw e))))))

(defn words
  "argv of a single simple command, or nil when s is not exactly one."
  ([s] (words :posix s))
  ([dialect s]
   (let [{:keys [pipeline]} (parse dialect s)]
     (when (= 1 (count pipeline)) (first pipeline)))))
