;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.lang.php
  "PHP as a target language, as data in the shapes of grammar.realize and
  grammar.probe.

  realizations  :lit, :str and :write-file rendered in :php. Each one
                requires :php.fn/<f> for every PHP function its text calls,
                so a function removed with `-d disable_functions=...` takes
                the realizations that call it out of the search. Language
                constructs (echo, eval, exit) are not functions and cannot
                be disabled; they require nothing.
  wrappers      context emitters into and out of PHP: sh running php -r,
                php eval, php running sh or python3 through proc_open,
                python running php through subprocess.
  probes        :interp/php, :php/first-class-callable, :php/proc-open-argv,
                then one function_exists probe per function the realizations
                and wrappers call. Each needs :interp/php, so without php
                they are skipped and stay :unknown.

  `module` bundles the three for grammar.core/compose, and
  grammar.core/modules includes it by default."
  (:require [clojure.string :as str]
            [grammar.candidate :as c]))

;; ---------------------------------------------------------------- literal encoders

(defn php-single
  "PHP single-quoted string. Only \\\\ and \\' are escapes inside; every
  other byte is literal. nil for NUL: program text travels as an argv word."
  [^String s]
  (when-not (str/includes? s "\u0000")
    (str "'" (str/replace s #"[\\']" #(str "\\" %)) "'")))

(defn php-chr
  "PHP expression with no quote characters at all: the UTF-8 bytes of s
  mapped through chr and joined. Carries any byte, NUL included."
  [^String s]
  (let [bs (.getBytes s "UTF-8")]
    (if (zero? (alength bs))
      "implode([])"
      (str "implode(array_map(chr(...),["
           (str/join "," (map #(bit-and (int %) 0xff) bs))
           "]))"))))

;; ---------------------------------------------------------------- capabilities

(defn fn-cap
  "Capability naming PHP function f: :php.fn/<f>."
  [f]
  (keyword "php.fn" (name f)))

(defn- fns [& fs] (into #{} (map fn-cap) fs))

;; ---------------------------------------------------------------- realizations

(defn- lit-in [env lang s] ((:lit env) lang s))

(defn- leaf-lit [encode] (fn [_ [_ s]] (c/leaf (encode s))))

(defn- fopen-write
  "Render fopen + writer + fclose. Exit status 1 when any step fails or the
  writer reports fewer bytes than the content holds: fwrite and fputs return
  a short count, not false, on a partial write."
  [writer]
  (fn [env [_ {:keys [path content]}]]
    (c/combine (fn [p m x]
                 (str "$c=" x ";$h=fopen(" p "," m ");"
                      "exit($h===false||" writer "($h,$c)!==strlen($c)||!fclose($h)?1:0);"))
               (lit-in env :php path) (lit-in env :php "wb") (lit-in env :php content))))

(def realizations
  [;; ------------------------------------------------ literals
   {:id :php.lit/single :intent :lit :lang :php :cost 1
    :requires #{} :render (leaf-lit php-single)}
   {:id :php.lit/chr :intent :lit :lang :php :cost 5
    :requires (conj (fns "implode" "array_map" "chr") :php/first-class-callable)
    :doc "No quote characters, any byte: for profiles that forbid quotes, and for NUL."
    :render (leaf-lit php-chr)}

   ;; ------------------------------------------------ :str
   {:id :php.str/fwrite :intent :str :lang :php :cost 1
    :requires (fns "fwrite" "strlen")
    :doc "Exit status 1 unless every byte was written: fwrite returns a short count, not false, on a partial write."
    :render (fn [env [_ s]]
              (c/combine #(str "$c=" % ";exit(fwrite(STDOUT,$c)===strlen($c)?0:1);") (lit-in env :php s)))}
   {:id :php.str/echo :intent :str :lang :php :cost 2
    :requires #{}
    :doc "echo is a language construct: disable_functions cannot remove it."
    :render (fn [env [_ s]]
              (c/combine #(str "echo " % ";") (lit-in env :php s)))}

   ;; ------------------------------------------------ :write-file
   {:id :php.write-file/put-contents :intent :write-file :lang :php :cost 1
    :requires (fns "file_put_contents")
    :doc "file_put_contents returns false on a short write."
    :render (fn [env [_ {:keys [path content]}]]
              (c/combine (fn [p x] (str "exit(file_put_contents(" p "," x ")===false?1:0);"))
                         (lit-in env :php path) (lit-in env :php content)))}
   {:id :php.write-file/fopen-fwrite :intent :write-file :lang :php :cost 2
    :requires (fns "fopen" "fwrite" "fclose" "strlen")
    :doc "For runtimes that disable file_put_contents."
    :render (fopen-write "fwrite")}
   {:id :php.write-file/fopen-fputs :intent :write-file :lang :php :cost 3
    :requires (fns "fopen" "fputs" "fclose" "strlen")
    :doc "fputs is an alias of fwrite that disable_functions treats as a separate function."
    :render (fopen-write "fputs")}])

;; ---------------------------------------------------------------- wrappers

(defn- proc-open
  "PHP running [prog flag code] as a child process, exiting with its status."
  [env prog flag code]
  (c/combine (fn [a b x]
               (str "exit(proc_close(proc_open([" a "," b "," x "],[STDIN,STDOUT,STDERR],$p)));"))
             (lit-in env :php prog) (lit-in env :php flag) (lit-in env :php code)))

(def wrappers
  [{:id :ctx/sh->php :outer :sh :inner :php :cost 11
    :requires #{:interp/php}
    :doc "Costs one more than :ctx/sh->python, so when inner costs tie delegation prefers python3 by cost, not by id spelling. Costs sum with the inner program's, so a cheaper inner php program can still win."
    :render (fn [env code] (c/combine #(str "php -r " %) (lit-in env :sh code)))}
   {:id :ctx/php->php :outer :php :inner :php :cost 5
    :requires #{}
    :doc "eval is a language construct."
    :render (fn [env code] (c/combine #(str "eval(" % ");") (lit-in env :php code)))}
   {:id :ctx/php->sh :outer :php :inner :sh :cost 10
    :requires (conj (fns "proc_open" "proc_close") :php/proc-open-argv :interp/sh)
    :render (fn [env code] (proc-open env "sh" "-c" code))}
   {:id :ctx/php->python :outer :php :inner :python :cost 10
    :requires (conj (fns "proc_open" "proc_close") :php/proc-open-argv :interp/python3)
    :render (fn [env code] (proc-open env "python3" "-c" code))}
   {:id :ctx/python->php :outer :python :inner :php :cost 11
    :requires #{:py/subprocess :interp/php}
    :doc "Costs one more than :ctx/python->sh, for the same reason as :ctx/sh->php."
    :render (fn [env code]
              (c/combine (fn [a b x] (str "import subprocess,sys;sys.exit(subprocess.call([" a "," b "," x "]))"))
                         (lit-in env :python "php") (lit-in env :python "-r") (lit-in env :python code)))}])

;; ---------------------------------------------------------------- probes

(def functions
  "Every PHP function the realizations and wrappers call, sorted."
  (->> (concat realizations wrappers)
       (mapcat :requires)
       (filter #(= "php.fn" (namespace %)))
       (map name)
       distinct
       sort
       vec))

(defn- fn-probe [f]
  {:cap (fn-cap f) :needs [:interp/php :sh/single-quote]
   :script (str "php -r 'echo function_exists(\"" f "\")?\"ok\":\"no\";'")
   :expect "ok"})

(def probes
  "Run after grammar.probe/probes, which proves the :sh/single-quote these need.
  A function disabled with disable_functions answers function_exists false,
  so its probe prints no and the verdict is :no."
  (into [{:cap :interp/php :needs [:sh/single-quote]
          :script "php -r 'echo \"ok\";'" :expect "ok"}
         {:cap :php/first-class-callable :needs [:interp/php :sh/single-quote]
          :script "php -r 'function f(){return \"ok\";}$g=f(...);echo $g();'" :expect "ok"}
         {:cap :php/proc-open-argv :needs [:interp/php :sh/single-quote]
          :doc "proc_open takes an argv array (no shell) from PHP 7.4 on; before, it returns null and proc_close(null) would exit 0."
          :script "php -r 'echo PHP_VERSION_ID>=70400?\"ok\":\"no\";'" :expect "ok"}]
        (map fn-probe)
        functions))

(def module
  "PHP as a grammar.core language module. Its parts are vars, so reloading
  this namespace is seen without reloading grammar.core. Its probes need
  :sh/single-quote, so it goes after the core module."
  {:id :php
   :realizations #'realizations
   :wrappers #'wrappers
   :probes #'probes})
