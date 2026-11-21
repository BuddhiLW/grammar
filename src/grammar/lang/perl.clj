;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.lang.perl
  "Perl as a target language: literal encoders, realizations, context
  wrappers and the probes behind every capability they rely on.

  Exports data in exactly the shapes of grammar.realize and grammar.probe.
  `module` bundles them for grammar.core/compose, and grammar.core/modules
  includes it by default.

  Strings are bytes. A Perl literal here denotes the UTF-8 bytes of the
  intent's string as a byte string (every char < 256, no utf8 flag), and
  every handle written to is binmode'd first, so no I/O layer (PERL_UNICODE,
  -C, a locale) can re-encode what is printed.

  Capabilities (all probed through the shell, all needing :interp/perl):

    :interp/perl     perl -e runs a program from the PATH
    :pl/binmode      binmode(STDOUT) makes STDOUT a raw byte stream
    :pl/print        print(STDOUT ...) writes and reports success
    :pl/syswrite     syswrite writes unbuffered and returns a byte count
    :pl/open-write   3-arg open(..., '>', path) creates/truncates a file
    :pl/sysopen      Fcntl loads and sysopen(O_WRONLY|O_CREAT|O_TRUNC) works
    :pl/system       system with a LIST runs a program (no shell) and waits"
  (:require [clojure.string :as str]
            [grammar.candidate :as c]))

;; ---------------------------------------------------------------- literal encoders

(defn- nul? [^String s] (str/includes? s "\u0000"))

(defn- utf8-bytes [^String s] (map #(bit-and (int %) 0xff) (.getBytes s "UTF-8")))

(defn perl-single
  "Perl single-quoted literal of ASCII text. Nothing interpolates; only \\
  and ' are escaped. nil when s holds:
  - NUL: program text travels as an argv word, which cannot carry one;
  - CR LF: perl's lexer drops a CR before LF in program text, inside quotes
    too, and a single-quoted string has no escape for CR;
  - any non-ASCII char: raw UTF-8 source bytes are read as bytes only
    without `use utf8`, and the environment can turn that on
    (PERL5OPT=-Mutf8), which would decode them into one char each.
    perl-double's \\x{hh} escapes stay bytes either way."
  [s]
  (when-not (or (nul? s) (str/includes? s "\r\n") (some #(< 127 (int %)) s))
    (str "'" (str/replace s #"[\\']" #(str "\\" %)) "'")))

(defn perl-double
  "Perl double-quoted literal of printable ASCII. Every byte that could
  interpolate or terminate ($ @ \\ \") and every control or non-ASCII byte
  becomes \\x{hh}, so NUL is fine and nothing interpolates."
  [s]
  (str "\""
       (apply str (map (fn [b]
                         (if (and (<= 32 b 126) (not (#{34 36 64 92} b)))
                           (str (char b))
                           (format "\\x{%02x}" b)))
                       (utf8-bytes s)))
       "\""))

(defn perl-chr
  "Perl expression with no quote characters at all: the UTF-8 bytes joined
  from chr. Carries NUL. The empty string is q()."
  [s]
  (if-let [bs (seq (utf8-bytes s))]
    (str "join(q(),map(chr," (str/join "," bs) "))")
    "q()"))

;; ---------------------------------------------------------------- realizations

(defn- lit-in [env lang s] ((:lit env) lang s))

(defn- leaf-lit [encode] (fn [_ [_ s]] (c/leaf (encode s))))

(defn- write-all
  "Perl statement writing all of $s to handle h with syswrite, which may
  write fewer bytes than asked."
  [h]
  (str "while(length($s)){my $n=syswrite(" h ",$s);defined($n) or exit(1);substr($s,0,$n,q())}"))

(def realizations
  [;; ------------------------------------------------ literals: perl
   {:id :pl.lit/single :intent :lit :lang :perl :cost 1
    :requires #{} :render (leaf-lit perl-single)}
   {:id :pl.lit/double :intent :lit :lang :perl :cost 2
    :requires #{} :render (leaf-lit perl-double)}
   {:id :pl.lit/chr :intent :lit :lang :perl :cost 5
    :requires #{} :render (leaf-lit perl-chr)}

   ;; ------------------------------------------------ :str  (emit exactly s on stdout)
   {:id :pl.str/print :intent :str :lang :perl :cost 1
    :requires #{:pl/binmode :pl/print}
    :render (fn [env [_ s]]
              (c/combine #(str "binmode(STDOUT);print(STDOUT " % ") or exit(1);close(STDOUT) or exit(1)")
                         (lit-in env :perl s)))}
   {:id :pl.str/syswrite :intent :str :lang :perl :cost 2
    :requires #{:pl/binmode :pl/syswrite}
    :doc "Unbuffered: loops until every byte is written."
    :render (fn [env [_ s]]
              (c/combine #(str "binmode(STDOUT);my $s=" % ";" (write-all "STDOUT"))
                         (lit-in env :perl s)))}

   ;; ------------------------------------------------ :write-file  (path holds exactly content)
   {:id :pl.write-file/open :intent :write-file :lang :perl :cost 1
    :requires #{:pl/open-write :pl/binmode :pl/print}
    :doc "3-arg open: the path is never parsed for a mode or a pipe."
    :render (fn [env [_ {:keys [path content]}]]
              (c/combine (fn [m p cnt]
                           (str "open(my $f," m "," p ") or exit(1);binmode($f);"
                                "print({$f} " cnt ") or exit(1);close($f) or exit(1)"))
                         (lit-in env :perl ">") (lit-in env :perl path) (lit-in env :perl content)))}
   {:id :pl.write-file/sysopen :intent :write-file :lang :perl :cost 3
    :requires #{:pl/sysopen :pl/binmode :pl/syswrite}
    :render (fn [env [_ {:keys [path content]}]]
              (c/combine (fn [p cnt]
                           (str "use Fcntl;sysopen(my $f," p ",O_WRONLY|O_CREAT|O_TRUNC) or exit(1);"
                                "binmode($f);my $s=" cnt ";" (write-all "$f") "close($f) or exit(1)"))
                         (lit-in env :perl path) (lit-in env :perl content)))}])

(def wrappers
  [{:id :ctx/sh->perl :outer :sh :inner :perl :cost 11
    :requires #{:interp/perl}
    :doc "Costs one more than :ctx/sh->python, so delegation prefers python3 by cost, not by id spelling."
    :render (fn [env code] (c/combine #(str "perl -e " %) (lit-in env :sh code)))}
   {:id :ctx/perl->sh :outer :perl :inner :sh :cost 10
    :requires #{:pl/system :interp/sh}
    :doc "system with a list: no shell parses the arguments. Exit status passes through."
    :render (fn [env code]
              (c/combine (fn [a b cd]
                           (str "system(" a "," b "," cd ");"
                                "exit($?==-1?127:($?&127)?128+($?&127):$?>>8)"))
                         (lit-in env :perl "sh") (lit-in env :perl "-c") (lit-in env :perl code)))}])

;; ---------------------------------------------------------------- probes

(def probes
  "Shell scripts, single-quoted perl inside. Run after grammar.probe/probes
  (:sh/single-quote is probed there); every :pl/ probe needs :interp/perl,
  so with perl absent they are skipped and stay :unknown."
  [{:cap :interp/perl    :needs [:sh/single-quote]
    :script "perl -e 'print(q(ok))'" :expect "ok"}
   {:cap :pl/binmode     :needs [:interp/perl :sh/single-quote]
    :script "perl -e 'binmode(STDOUT) or exit(1);print(STDOUT chr(111),chr(107))'" :expect "ok"}
   {:cap :pl/print       :needs [:interp/perl :sh/single-quote]
    :script "perl -e 'print(STDOUT q(ok)) or exit(1);close(STDOUT) or exit(1)'" :expect "ok"}
   {:cap :pl/syswrite    :needs [:interp/perl :sh/single-quote]
    :script "perl -e 'binmode(STDOUT) or exit(1);syswrite(STDOUT,q(ok))==2 or exit(1)'" :expect "ok"}
   {:cap :pl/open-write  :needs [:interp/perl :sh/single-quote]
    :script "perl -e 'open(my $f,q(>),q(/dev/null)) or exit(1);binmode($f);print({$f} q(x)) or exit(1);close($f) or exit(1);print(q(ok))'"
    :expect "ok"}
   {:cap :pl/sysopen     :needs [:interp/perl :sh/single-quote]
    :script "perl -e 'use Fcntl;sysopen(my $f,q(/dev/null),O_WRONLY|O_CREAT|O_TRUNC) or exit(1);close($f) or exit(1);print(q(ok))'"
    :expect "ok"}
   {:cap :pl/system      :needs [:interp/perl :sh/single-quote]
    :script "perl -e 'exit(system($^X,q(-e),q(print(q(ok)))) == 0 ? 0 : 1)'" :expect "ok"}])

(def module
  "Perl as a grammar.core language module. Its parts are vars, so reloading
  this namespace is seen without reloading grammar.core. Its probes need
  :sh/single-quote, so it goes after the core module."
  {:id :perl
   :realizations #'realizations
   :wrappers #'wrappers
   :probes #'probes})
