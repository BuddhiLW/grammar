;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.realize
  "Built-in realizations and context wrappers, as data.

  A realization renders one intent in one language. It declares every
  capability its text relies on, and the solver admits it only when the
  profile holds evidence that each one is :yes.

  A wrapper is a context emitter: it turns a program in an inner language
  into a program in an outer one (sh running python3 -c, python running
  sh -c, ...). Wrappers build context stacks, both for automatic
  delegation and for an explicitly requested stack.

  :render receives an env whose (:lit env) is (fn [lang s] candidates) and
  returns a lazy seq of candidates (see grammar.candidate)."
  (:require [grammar.candidate :as c]
            [grammar.lit :as lit]
            [clojure.string :as str]))

(defn- lit-in [env lang s] ((:lit env) lang s))

(defn- leaf-lit [encode] (fn [_ [_ s]] (c/leaf (encode s))))

(defn bin-cap
  "Capability: program name resolves to an executable on the runtime's PATH."
  [program]
  (keyword "bin" program))

(defn- run-requires [[_ {:keys [argv]}]] #{(bin-cap (first argv))})

(def realizations
  [;; ------------------------------------------------ literals: sh
   {:id :sh.lit/single :intent :lit :lang :sh :cost 1
    :requires #{:sh/single-quote} :render (leaf-lit lit/sh-single)}
   {:id :sh.lit/double :intent :lit :lang :sh :cost 2
    :requires #{:sh/double-quote} :render (leaf-lit lit/sh-double)}
   {:id :sh.lit/ansi-c :intent :lit :lang :sh :cost 3
    :requires #{:sh/ansi-c-quote} :render (leaf-lit lit/sh-ansi-c)}

   ;; ------------------------------------------------ literals: python
   {:id :py.lit/single :intent :lit :lang :python :cost 1
    :requires #{} :render (leaf-lit lit/py-single)}
   {:id :py.lit/double :intent :lit :lang :python :cost 2
    :requires #{} :render (leaf-lit lit/py-double)}
   {:id :py.lit/chr :intent :lit :lang :python :cost 5
    :requires #{} :render (leaf-lit lit/py-chr)}

   ;; ------------------------------------------------ :str  (emit exactly s on stdout)
   {:id :sh.str/printf :intent :str :lang :sh :cost 1
    :requires #{:sh/printf}
    :render (fn [env [_ s]]
              (c/combine #(str "printf %s " %) (lit-in env :sh s)))}
   {:id :sh.str/printf-octal :intent :str :lang :sh :cost 4
    :requires #{:sh/printf}
    :doc "Any bytes, NUL included: the payload rides in the FORMAT operand."
    :render (fn [env [_ s]]
              (c/combine #(str "printf " %) (lit-in env :sh (lit/printf-format s))))}
   {:id :py.str/stdout :intent :str :lang :python :cost 1
    :requires #{:py/sys}
    :render (fn [env [_ s]]
              (c/combine #(str "import sys;sys.stdout.buffer.write(" % ".encode())")
                         (lit-in env :python s)))}

   ;; ------------------------------------------------ :write-file  (path holds exactly content)
   {:id :sh.write-file/redirect :intent :write-file :lang :sh :cost 1
    :requires #{:sh/printf :sh/redirect}
    :render (fn [env [_ {:keys [path content]}]]
              (c/combine (fn [c p] (str "printf %s " c " >" p))
                         (lit-in env :sh content) (lit-in env :sh path)))}
   {:id :sh.write-file/tee :intent :write-file :lang :sh :cost 3
    :requires #{:sh/printf :sh/pipe :bin/tee}
    :doc "For shells that refuse output redirection. tee also copies content to stdout."
    :render (fn [env [_ {:keys [path content]}]]
              (c/combine (fn [c p] (str "printf %s " c " | tee -- " p))
                         (lit-in env :sh content) (lit-in env :sh path)))}
   {:id :sh.write-file/redirect-octal :intent :write-file :lang :sh :cost 4
    :requires #{:sh/printf :sh/redirect}
    :doc "Content as a printf FORMAT: NUL-safe, and quote characters become octal."
    :render (fn [env [_ {:keys [path content]}]]
              (c/combine (fn [f p] (str "printf " f " >" p))
                         (lit-in env :sh (lit/printf-format content)) (lit-in env :sh path)))}
   {:id :py.write-file/pathlib :intent :write-file :lang :python :cost 1
    :requires #{:py/pathlib}
    :render (fn [env [_ {:keys [path content]}]]
              (c/combine (fn [p c] (str "import pathlib;pathlib.Path(" p ").write_bytes(" c ".encode())"))
                         (lit-in env :python path) (lit-in env :python content)))}

   ;; ------------------------------------------------ :run  (exec argv, every word literal)
   {:id :sh.run/words :intent :run :lang :sh :cost 1
    :requires #{} :requires-of run-requires
    :doc "One simple command; each argv element is one quoted word."
    :render (fn [env [_ {:keys [argv]}]]
              (apply c/combine (fn [& ws] (str/join " " ws))
                     (map #(lit-in env :sh %) argv)))}
   {:id :py.run/subprocess :intent :run :lang :python :cost 1
    :requires #{:py/subprocess} :requires-of run-requires
    :render (fn [env [_ {:keys [argv]}]]
              (apply c/combine
                     (fn [& ws] (str "import subprocess,sys;sys.exit(subprocess.call([" (str/join "," ws) "]))"))
                     (map #(lit-in env :python %) argv)))}])

(def wrappers
  [{:id :ctx/sh->sh :outer :sh :inner :sh :cost 5
    :requires #{:interp/sh}
    :render (fn [env code] (c/combine #(str "sh -c " %) (lit-in env :sh code)))}
   {:id :ctx/sh->python :outer :sh :inner :python :cost 10
    :requires #{:interp/python3}
    :render (fn [env code] (c/combine #(str "python3 -c " %) (lit-in env :sh code)))}
   {:id :ctx/python->python :outer :python :inner :python :cost 5
    :requires #{}
    :render (fn [env code] (c/combine #(str "exec(" % ")") (lit-in env :python code)))}
   {:id :ctx/python->sh :outer :python :inner :sh :cost 10
    :requires #{:py/subprocess :interp/sh}
    :render (fn [env code]
              (c/combine (fn [a b c] (str "import subprocess,sys;sys.exit(subprocess.call([" a "," b "," c "]))"))
                         (lit-in env :python "sh") (lit-in env :python "-c") (lit-in env :python code)))}])
