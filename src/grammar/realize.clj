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
  returns a lazy seq of candidates (see grammar.candidate), ascending by
  :cost: best-first takes the first one's cost as a lower bound for the
  rest. grammar.candidate/leaf and grammar.candidate/combine both return
  such seqs. A realization's :cost and a wrapper's :cost are numbers >= 0
  and every :id is a keyword; grammar.solve/make-env refuses anything else."
  (:require [grammar.candidate :as c]
            [grammar.lit :as lit]
            [clojure.string :as str]))

(defn- lit-in [env lang s] ((:lit env) lang s))

(defn- leaf-lit [encode] (fn [_ [_ s]] (c/leaf (encode s))))

(defn bin-cap
  "Capability: exec finds program as an executable file on PATH (what
  python's subprocess needs)."
  [program]
  (keyword "bin" program))

(defn cmd-cap
  "Capability: the shell resolves program as a command word, builtins and
  applets included (what an sh simple command needs)."
  [program]
  (keyword "sh.cmd" program))

(defn- program [[_ {:keys [argv]}]] (first argv))

(defn- sh-words
  "Candidates for one simple command: each element of words one literal word."
  [env words]
  (apply c/combine (fn [& ws] (str/join " " ws)) (map #(lit-in env :sh %) words)))

(defn- http-only?
  "True for a plain http:// URL: what a fetcher that cannot verify a TLS
  certificate may serve."
  [url]
  (str/starts-with? url "http://"))

(defn py-bytes
  "python expression for the UTF-8 bytes of python str expression e. An
  argv word handed to subprocess as bytes is never encoded with the
  locale, so a non-ASCII word survives LC_ALL=C with PYTHONUTF8=0.
  str.encode() with no argument is UTF-8 whatever the locale, and adds no
  quote character, so a policy forbidding quotes still admits it."
  [e]
  (str "(" e ").encode()"))

(defn- py-path
  "python expression for a pathlib.Path naming the UTF-8 bytes of python str
  expression e, decoded with os.fsdecode: under an ASCII locale the
  non-ASCII bytes ride as surrogate escapes and reach the OS unchanged."
  [e]
  (str "pathlib.Path(os.fsdecode(" (py-bytes e) "))"))

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
    :requires #{:py/sys :py/stdout-buffer}
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
    :requires #{:py/pathlib :py/open-write}
    :doc "write_bytes opens path in write mode: a runtime whose audit hook refuses that refutes :py/open-write."
    :render (fn [env [_ {:keys [path content]}]]
              (c/combine (fn [p c] (str "import os,pathlib;" (py-path p) ".write_bytes(" (py-bytes c) ")"))
                         (lit-in env :python path) (lit-in env :python content)))}

   ;; ------------------------------------------------ :run  (exec argv, every word literal)
   {:id :sh.run/words :intent :run :lang :sh :cost 1
    :requires #{} :requires-of #(hash-set (cmd-cap (program %)))
    :doc "One simple command; each argv element is one quoted word."
    :render (fn [env [_ {:keys [argv]}]] (sh-words env argv))}
   {:id :py.run/subprocess :intent :run :lang :python :cost 1
    :requires #{:py/subprocess} :requires-of #(hash-set (bin-cap (program %)))
    :doc "Each argv word is passed as UTF-8 bytes, so no locale (LC_ALL=C, PYTHONUTF8=0) can make subprocess refuse a non-ASCII word."
    :render (fn [env [_ {:keys [argv]}]]
              (apply c/combine
                     (fn [& ws] (str "import subprocess,sys;sys.exit(subprocess.call([" (str/join "," (map py-bytes ws)) "]))"))
                     (map #(lit-in env :python %) argv)))}

   ;; ------------------------------------------------ :ensure-dir  (path is a directory, parents too)
   {:id :sh.ensure-dir/mkdir :intent :ensure-dir :lang :sh :cost 1
    :requires #{:sh.cmd/mkdir}
    :render (fn [env [_ {:keys [path]}]] (sh-words env ["mkdir" "-p" "--" path]))}
   {:id :py.ensure-dir/pathlib :intent :ensure-dir :lang :python :cost 1
    :requires #{:py/pathlib :py.os/mkdir}
    :render (fn [env [_ {:keys [path]}]]
              (c/combine #(str "import os,pathlib;" (py-path %) ".mkdir(parents=True,exist_ok=True)")
                         (lit-in env :python path)))}

   ;; ------------------------------------------------ :remove-file  (path no longer exists as a file)
   {:id :sh.remove-file/rm :intent :remove-file :lang :sh :cost 1
    :requires #{:sh.cmd/rm}
    :render (fn [env [_ {:keys [path]}]] (sh-words env ["rm" "-f" "--" path]))}
   {:id :py.remove-file/pathlib :intent :remove-file :lang :python :cost 1
    :requires #{:py/pathlib :py.os/unlink}
    :render (fn [env [_ {:keys [path]}]]
              (c/combine #(str "import os,pathlib;" (py-path %) ".unlink(missing_ok=True)")
                         (lit-in env :python path)))}

   ;; ------------------------------------------------ :http-get  (response body on stdout, byte for byte)
   ;; Exit 0 on a 2xx answer, non-zero on a 4xx/5xx answer or a connection
   ;; failure, a body shorter than its Content-Length included. The URL is
   ;; one literal word after `--`; the Intent schema admits only http:// and
   ;; https://, so no URL can start with `-` anyway.
   ;;
   ;; https needs a fetcher that verifies the server certificate. busybox
   ;; wget does not (busybox 1.37 without openssl accepts a self-signed
   ;; certificate, noting "TLS certificate validation not implemented"), so
   ;; a wget not proven to be GNU Wget, and busybox wget, decline https URLs:
   ;; they render no candidate for them.
   {:id :sh.http-get/curl :intent :http-get :lang :sh :cost 1
    :requires #{:sh.cmd/curl}
    :doc "-q (first) skips .curlrc; -f fails on 4xx/5xx; -sS quiet but reports errors; -L follows redirects; -g turns URL globbing off, so [1-2] or {a,b} in a URL is one request, not several. A short body exits 18."
    :render (fn [env [_ {:keys [url]}]] (sh-words env ["curl" "-q" "-fsSLg" "--" url]))}
   {:id :sh.http-get/gnu-wget :intent :http-get :lang :sh :cost 2
    :requires #{:sh.cmd/wget :sh.wget/gnu}
    :doc "GNU Wget, proven by its --version banner: verifies certificates, so it serves https too. --no-config (first) skips any wgetrc, which could turn check_certificate off; --tries=1 makes a short body exit 4 instead of retrying with Range requests."
    :render (fn [env [_ {:keys [url]}]] (sh-words env ["wget" "--no-config" "--tries=1" "-qO-" "--" url]))}
   {:id :sh.http-get/wget :intent :http-get :lang :sh :cost 3
    :requires #{:sh.cmd/wget}
    :doc "A wget not proven GNU, typically the busybox applet: http only. The flags and `--` mean the same to busybox and GNU."
    :render (fn [env [_ {:keys [url]}]]
              (when (http-only? url) (sh-words env ["wget" "-qO-" "--" url])))}
   {:id :sh.http-get/busybox-wget :intent :http-get :lang :sh :cost 4
    :requires #{:sh.cmd/busybox}
    :doc "The wget applet through the busybox binary, for a PATH with busybox but no wget: http only. busybox wget accepts `--` (getopt), checked against busybox 1.37."
    :render (fn [env [_ {:keys [url]}]]
              (when (http-only? url) (sh-words env ["busybox" "wget" "-qO-" "--" url])))}
   {:id :py.http-get/urllib :intent :http-get :lang :python :cost 1
    :requires #{:py/urllib :py/stdout-buffer}
    :doc "urlopen follows 301, 302, 303 and 307 redirects (308 from Python 3.11 on), raises HTTPError for a 4xx/5xx answer and URLError when it cannot connect. A body shorter than its Content-Length leaves r.length non-zero (read(amt) returns b'' instead of raising IncompleteRead), so that raises too. sys.exit(repr(e)) prints the error on stderr and exits 1. The program text holds no quote character of its own."
    :render (fn [env [_ {:keys [url]}]]
              (c/combine #(str "import shutil,sys,urllib.request\n"
                               "try:\n"
                               " r=urllib.request.urlopen(" % ");shutil.copyfileobj(r,sys.stdout.buffer)\n"
                               " if r.length:raise EOFError(r.length)\n"
                               "except Exception as e:sys.exit(repr(e))")
                         (lit-in env :python url)))}])

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
              (c/combine (fn [a b c] (str "import subprocess,sys;sys.exit(subprocess.call([" (str/join "," (map py-bytes [a b c])) "]))"))
                         (lit-in env :python "sh") (lit-in env :python "-c") (lit-in env :python code)))}])
