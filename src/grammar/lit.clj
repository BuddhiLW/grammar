;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.lit
  "Pure literal encoders.

  Each encoder returns program text that, read in its context, denotes
  exactly the input string, or nil when that context cannot denote it at
  all (a shell word cannot hold NUL). Encoders never consult a profile:
  whether a construct is allowed is the solver's business."
  (:require [clojure.string :as str]))

(defn- nul? [^String s] (str/includes? s "\u0000"))

(defn- escape-chars
  "Map each char of s through f (char -> string)."
  [f ^String s]
  (apply str (map f s)))

;; ---------------------------------------------------------------- sh

(defn sh-single
  "POSIX single quotes. Everything is literal except ', spelled '\\''."
  [s]
  (when-not (nul? s)
    (str "'" (str/replace s "'" "'\\''") "'")))

(def ^:private sh-double-special #{\\ \" \$ \`})

(defn sh-double
  "POSIX double quotes. Only \\ \" $ ` are special inside; escape those."
  [s]
  (when-not (nul? s)
    (str "\""
         (escape-chars (fn [c] (if (sh-double-special c) (str "\\" c) (str c))) s)
         "\"")))

(defn sh-ansi-c
  "bash/ksh/zsh $'...' quoting. Control characters as fixed-width octal so
  a following digit can never extend the escape."
  [s]
  (when-not (nul? s)
    (str "$'"
         (escape-chars (fn [c]
                         (let [i (int c)]
                           (cond (= c \\) "\\\\"
                                 (= c \') "\\'"
                                 (or (< i 32) (= i 127)) (format "\\%03o" i)
                                 :else (str c))))
                       s)
         "'")))

(defn printf-format-bytes
  "A printf FORMAT operand whose output is exactly the bytes bs (a byte
  array or a seq of byte values), NUL and invalid UTF-8 included. See
  printf-format."
  [bs]
  (apply str
         (map-indexed
          (fn [i b]
            (let [u (bit-and (int b) 0xff)]
              (cond (and (zero? i) (= u 45)) "\\055"
                    (= u 37) "%%"
                    (= u 92) "\\\\"
                    (#{34 36 39 96} u) (format "\\%03o" u)
                    (<= 32 u 126) (str (char u))
                    :else (format "\\%03o" u))))
          bs)))

(defn printf-format
  "A printf FORMAT operand whose output is exactly the UTF-8 bytes of s, NUL
  included. The operand is printable ASCII with no shell quote or expansion
  characters (' \" $ ` become octal), so it survives any quoting context and
  any forbidden-quote policy. A leading '-' is escaped so no printf can
  mistake the operand for an option."
  [^String s]
  (printf-format-bytes (.getBytes s "UTF-8")))

;; ---------------------------------------------------------------- python

(defn- code-points [^String s] (seq (.toArray (.codePoints s))))

(defn- py-escape [quote cp]
  (cond (= cp 92) "\\\\"
        (= cp (int quote)) (str "\\" quote)
        (<= 32 cp 126) (str (char cp))
        (<= cp 0xff) (format "\\x%02x" cp)
        (<= cp 0xffff) (format "\\u%04x" cp)
        :else (format "\\U%08x" cp)))

(defn- py-quoted [quote s]
  (str quote (apply str (map #(py-escape quote %) (code-points s))) quote))

(defn py-single
  "Python str literal in single quotes; output is printable ASCII."
  [s]
  (py-quoted \' s))

(defn py-double
  "Python str literal in double quotes; output is printable ASCII."
  [s]
  (py-quoted \" s))

(defn py-chr
  "Python expression with no quote characters at all."
  [s]
  (if-let [cps (code-points s)]
    (str "str().join(map(chr,[" (str/join "," cps) "]))")
    "str()"))
