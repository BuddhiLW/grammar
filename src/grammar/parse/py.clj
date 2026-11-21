;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.parse.py
  "Pure model of the python3 programs the planner emits as relays and
  leaves: string-literal expressions and the few statement shapes built
  around them. Anything else is refused, like grammar.parse.sh.

  Expressions: a quoted str literal ('..' or \"..\", escapes \\\\ \\' \\\"
  \\xhh \\uhhhh \\Uhhhhhhhh, printable ASCII otherwise) or
  str().join(map(chr,[n,...])) or str().

  Programs (parse-program):
    import sys;sys.stdout.buffer.write(E.encode())           {:op :write :text s}
    exec(E)                                                  {:op :exec :code s}
    import subprocess,sys;sys.exit(subprocess.call([E,..]))  {:op :call :argv [s ..]}"
  (:require [clojure.string :as str]))

(defn- refuse [msg]
  (throw (ex-info msg {::refuse true})))

(defn- hex-cp [^String s i n]
  (when (> (+ i n) (count s)) (refuse "short escape"))
  (let [h (subs s i (+ i n))]
    (when-not (re-matches #"[0-9a-fA-F]+" h) (refuse "bad hex escape"))
    (Long/parseLong h 16)))

(defn- quoted
  "[string rest] for the str literal at the start of s."
  [^String s]
  (let [q (first s)]
    (when-not (#{\' \"} q) (refuse "not a str literal"))
    (loop [i 1 sb (StringBuilder.)]
      (when (>= i (count s)) (refuse "unterminated str literal"))
      (let [c (.charAt s i)]
        (cond
          (= c q) [(str sb) (subs s (inc i))]
          (= c \\) (let [n (when (< (inc i) (count s)) (.charAt s (inc i)))]
                     (case n
                       (\\ \' \") (recur (+ i 2) (.append sb n))
                       \x (recur (+ i 4) (.appendCodePoint sb (int (hex-cp s (+ i 2) 2))))
                       \u (recur (+ i 6) (.appendCodePoint sb (int (hex-cp s (+ i 2) 4))))
                       \U (recur (+ i 10) (.appendCodePoint sb (int (hex-cp s (+ i 2) 8))))
                       (refuse "escape not modeled")))
          (<= 32 (int c) 126) (recur (inc i) (.append sb c))
          :else (refuse "raw non-printable in literal"))))))

(defn expr
  "[string rest] for the str expression at the start of s."
  [^String s]
  (cond
    (str/starts-with? s "str().join(map(chr,[")
    (let [body (subs s (count "str().join(map(chr,["))
          end (str/index-of body "]))")]
      (when-not end (refuse "unterminated chr list"))
      (let [cps (map #(Long/parseLong %) (str/split (subs body 0 end) #","))
            sb (StringBuilder.)]
        (doseq [cp cps] (.appendCodePoint sb (int cp)))
        [(str sb) (subs body (+ end 3))]))
    (str/starts-with? s "str()") ["" (subs s 5)]
    :else (quoted s)))

(defn- expect-prefix [^String s p]
  (when-not (str/starts-with? s p) (refuse (str "expected " p)))
  (subs s (count p)))

(defn- parse* [^String s]
  (cond
    (str/starts-with? s "import sys;sys.stdout.buffer.write(")
    (let [[t r] (expr (subs s (count "import sys;sys.stdout.buffer.write(")))]
      (when-not (= r ".encode())") (refuse "trailing text"))
      {:op :write :text t})

    (str/starts-with? s "exec(")
    (let [[t r] (expr (subs s 5))]
      (when-not (= r ")") (refuse "trailing text"))
      {:op :exec :code t})

    (str/starts-with? s "import subprocess,sys;sys.exit(subprocess.call([")
    (loop [r (subs s (count "import subprocess,sys;sys.exit(subprocess.call([")) argv []]
      (let [[t r] (expr r)
            argv (conj argv t)]
        (cond (str/starts-with? r ",") (recur (subs r 1) argv)
              (= r "]))") {:op :call :argv argv}
              :else (refuse "malformed argv list"))))

    :else (refuse "program shape not modeled")))

(defn parse-program
  "The relay or leaf a python3 program text denotes, or {:error msg}."
  [s]
  (try (parse* s)
       (catch clojure.lang.ExceptionInfo e
         (if (::refuse (ex-data e)) {:error (ex-message e)} (throw e)))))
