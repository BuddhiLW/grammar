;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.lang.perl-test
  "Perl target against the real perl. Environments are composed explicitly
  (grammar.core still knows only :sh and :python; wiring it in is card
  GRAMMAR-SEAM). Runtimes are real interpreters with PATH cut down to a
  scratch bin dir, as in grammar.fixtures."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [grammar.core :as g]
            [grammar.fixtures :as fx]
            [grammar.lang.perl :as perl]
            [grammar.probe :as probe]
            [grammar.profile :as p]
            [grammar.realize :as realize]
            [grammar.roundtrip-test :as rtt]
            [grammar.runtime :as rt]
            [grammar.solve :as solve])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util Arrays]))

;; ---------------------------------------------------------------- runtimes

(def perl-bin
  "Host perl, canonical, so a symlink to it finds its own @INC: the linuxbrew
  perl when present (dev host), else the first perl on the host PATH (CI)."
  (let [brew (File. "/home/linuxbrew/.linuxbrew/bin/perl")]
    (or (when (.canExecute brew) (.getCanonicalPath brew))
        (fx/which "perl")
        (throw (ex-info "no perl on this host" {})))))

(def ^:private tools (assoc fx/tools :perl perl-bin))

(defn- bin-dir!
  "Fresh directory with a symlink for each tool in ks (perl included)."
  [ks]
  (let [dir (File. ^String (fx/tmp-dir! "grammar-perl-bin-"))]
    (doseq [k ks]
      (Files/createSymbolicLink (.toPath (File. dir (name k)))
                                (.toPath (File. ^String (tools k)))
                                (make-array FileAttribute 0)))
    (str dir)))

(defn- env-for [bin] {"PATH" bin "LC_ALL" "C.UTF-8"})

(defn- sh-rt [id shell bin]
  (rt/process-runtime {:id id :lang :sh :argv (fx/shells shell) :env (env-for bin)}))

(defn- perl-rt
  "A :perl runtime: perl -e PROGRAM."
  [id bin]
  (rt/process-runtime {:id id :lang :perl :argv [perl-bin "-e"] :env (env-for bin)}))

(def all-probes (into probe/probes perl/probes))

(defn- discover [runtime]
  (probe/discover (rt/runtime-id runtime) #(rt/execute runtime %) all-probes))

(defn- env [profile]
  (solve/make-env profile {:realizations (into realize/realizations perl/realizations)
                           :wrappers (into realize/wrappers perl/wrappers)}))

(defn- run-intent
  "Plan intent for runtime under profile (perl registry included), run it,
  verify it with grammar.core/verify. A non-:ok plan is returned unchanged."
  ([runtime profile intent] (run-intent runtime profile intent {}))
  ([runtime profile intent opts]
   (let [res (solve/solve (env profile) (rt/runtime-lang runtime) intent opts)]
     (if (= :ok (:status res))
       (let [run (rt/execute runtime (:code res))]
         (assoc res :execution (select-keys run [:exit :err :error])
                :verified (g/verify intent run)))
       res))))

(def ^:private rts
  (delay (let [full (bin-dir! [:sh :perl :python3])]
           {:dash (sh-rt :pl-dash :dash full)
            :bash (sh-rt :pl-bash :bash full)
            :perl (perl-rt :pl-perl full)})))

(def ^:private profiles
  (delay {:dash (discover (:dash @rts)) :bash (discover (:bash @rts))}))

;; ---------------------------------------------------------------- discovery

(deftest perl-probes-observe-the-real-perl
  (let [prof (:dash @profiles)]
    (doseq [{:keys [cap]} perl/probes]
      (testing (str cap)
        (is (= :yes (p/verdict prof cap)))
        (let [ev (get-in prof [:evidence (first (get-in prof [:caps cap :evidence]))])]
          (is (= 0 (:exit ev)))
          (is (= "ok" (:out ev))))))
    (testing "every :pl/ probe needs :interp/perl, which is probed first"
      (is (= :interp/perl (:cap (first perl/probes))))
      (is (every? #(some #{:interp/perl} (:needs %)) (rest perl/probes))))))

(deftest perl-absent-leaves-pl-caps-unknown
  (let [prof (discover (sh-rt :pl-noperl :dash (bin-dir! [:sh])))]
    (is (= :no (p/verdict prof :interp/perl)))
    (doseq [{:keys [cap]} (rest perl/probes)]
      (testing (str cap)
        (is (= :unknown (p/verdict prof cap)))
        (is (str/starts-with? (:reason (get-in prof [:evidence (first (get-in prof [:caps cap :evidence]))]))
                              "skipped: prerequisite not proven: :interp/perl=no"))))))

;; ---------------------------------------------------------------- literal round trips

(defn- perl-roundtrip?
  "Execute `print` of the literal in the real perl; stdout must be the exact
  UTF-8 bytes of s."
  [encode s]
  (let [run (rt/execute (:perl @rts) (str "binmode(STDOUT);print(STDOUT " (encode s) ") or exit(1)"))]
    (and (= 0 (:exit run)) (Arrays/equals (.getBytes ^String s "UTF-8") ^bytes (:out-bytes run)))))

(def gen-text-no-crlf
  "gen-text with every CR LF pair split, so CR and LF both still occur."
  (gen/fmap #(str/replace % "\r\n" "\r \n") rtt/gen-text))

(defspec perl-single-roundtrip 100 (prop/for-all [s gen-text-no-crlf] (perl-roundtrip? perl/perl-single s)))

(defspec perl-single-declines-exactly-nul-and-crlf 200
  (prop/for-all [s (gen/one-of [rtt/gen-text rtt/gen-text-nul
                                (gen/fmap (fn [[a b]] (str a "\r\n" b)) (gen/tuple rtt/gen-text rtt/gen-text))])]
    (= (nil? (perl/perl-single s))
       (or (str/includes? s "\u0000") (str/includes? s "\r\n")))))
(defspec perl-double-roundtrip 100 (prop/for-all [s rtt/gen-text-nul] (perl-roundtrip? perl/perl-double s)))
(defspec perl-chr-roundtrip 100 (prop/for-all [s rtt/gen-text-nul] (perl-roundtrip? perl/perl-chr s)))

(deftest encoder-shapes
  (is (nil? (perl/perl-single "a\u0000b")) "an argv word cannot carry NUL")
  (is (nil? (perl/perl-single "a\r\nb")) "perl drops CR before LF in program text")
  (is (some? (perl/perl-single "a\rb\n\r")))
  (is (perl-roundtrip? perl/perl-double "a\r\nb\r\r\n"))
  (is (perl-roundtrip? perl/perl-chr "a\r\nb\r\r\n"))
  (is (= "'it\\'s \\\\ $x @y'" (perl/perl-single "it's \\ $x @y")))
  (is (= "\"\\x{24}x\\x{40}y\\x{00}\\x{ce}\\x{bb}\"" (perl/perl-double "$x@y\u0000λ")))
  (is (= "q()" (perl/perl-chr "")))
  (doseq [s ["it's \"x\" $HOME" "λ\u0000🚀" ""]]
    (is (not-any? #(str/includes? (perl/perl-chr s) %) ["'" "\""]))
    (is (not-any? #(str/includes? (perl/perl-double s) %) ["$" "@"]))))

(defn- isolated-env
  "Env whose only perl :lit and :str realizations are lit-id and str-id, so
  the solver can only use that one encoder."
  [profile lit-id str-id]
  (solve/make-env profile {:realizations (into realize/realizations
                                               (remove #(and (= :perl (:lang %)) (#{:lit :str} (:intent %))
                                                             (not (#{lit-id str-id} (:id %)))))
                                               perl/realizations)
                           :wrappers (into realize/wrappers perl/wrappers)}))

(defn- str-via-perl? [keep str-id s]
  (let [res (solve/solve (isolated-env (:dash @profiles) keep str-id) :sh [:str s] {:stack [:sh :perl]})
        run (rt/execute (:dash @rts) (:code res))]
    (and (= :ok (:status res))
         (= [:ctx/sh->perl str-id keep] (vec (take 3 (:realization res))))
         (true? (:ok? (g/verify [:str s] run))))))

(defspec str-print-single-through-sh-perl 60 (prop/for-all [s gen-text-no-crlf] (str-via-perl? :pl.lit/single :pl.str/print s)))
(defspec str-syswrite-double-through-sh-perl 60 (prop/for-all [s rtt/gen-text] (str-via-perl? :pl.lit/double :pl.str/syswrite s)))
(defspec str-print-chr-through-sh-perl 60 (prop/for-all [s rtt/gen-text] (str-via-perl? :pl.lit/chr :pl.str/print s)))

(defspec nul-on-a-perl-runtime 100
  (prop/for-all [s rtt/gen-text-nul]
    (let [res (run-intent (:perl @rts) (:dash @profiles) [:str s])]
      (and (= :ok (:status res))
           (= [:pl.str/print :pl.lit/double] (:realization res))
           (true? (:ok? (:verified res)))))))

;; ---------------------------------------------------------------- context stacks

(def gen-stack
  "Launched from sh; inner layers any mix of :sh :python :perl."
  (gen/fmap #(into [:sh] %) (gen/vector (gen/elements [:sh :python :perl]) 0 3)))

(defn- reachable?
  "Every adjacent pair has a wrapper: none for python<->perl or perl->perl."
  [stack]
  (not-any? #{[:python :perl] [:perl :python] [:perl :perl]} (map vec (partition 2 1 stack))))

(defspec mixed-stacks-roundtrip 100
  (prop/for-all [stack (gen/such-that reachable? gen-stack 100)
                 s rtt/gen-text]
    (let [res (run-intent (:dash @rts) (:dash @profiles) [:str s] {:stack stack})]
      (and (= :ok (:status res))
           (= stack (:stack res))
           (true? (:ok? (:verified res)))))))

(deftest fixed-nasty-strings-through-perl-stacks
  (doseq [stack [[:sh :perl] [:sh :perl :sh] [:sh :perl :sh :perl] [:sh :perl :sh :python]
                 [:sh :python :sh :perl] [:sh :sh :perl :sh]]
          s ["" "'" "'\\''\"$`\\" "$x @y %h \\n" "%s%%-n\n\n" "λ🚀\u0001\u007f" "crlf\r\nend\r"]]
    (testing (pr-str stack s)
      (let [res (run-intent (:bash @rts) (:bash @profiles) [:str s] {:stack stack})]
        (is (= :ok (:status res)))
        (is (= stack (:stack res)))
        (is (true? (:ok? (:verified res))) (pr-str (:code res) (:execution res)))))))

(deftest perl-to-sh-passes-exit-status
  (let [res (solve/solve (env (:dash @profiles)) :sh [:str "x"] {:stack [:sh :perl :sh]})
        code (str/replace (:code res) "printf %s" "exit 7;")]
    (is (= [:ctx/sh->perl :ctx/perl->sh] (filterv #{:ctx/sh->perl :ctx/perl->sh} (:realization res))))
    (is (= 7 (:exit (rt/execute (:dash @rts) code))))))

;; ---------------------------------------------------------------- :write-file acceptance

(def content "it's \"x\" $HOME @ARGV `id` 100% -n\\\r\n\t\u0001λ 🚀\n")

(defn- evidence-linked? [profile res]
  (every? (fn [id]
            (let [ev (get-in profile [:evidence id])]
              (case (:kind ev)
                :probe (or (int? (:exit ev)) (:error ev) (:reason ev))
                :declared (string? (:reason ev))
                false)))
          (:evidence res)))

(defn- file-bytes [path]
  (let [f (File. ^String path)] (when (.isFile f) (Files/readAllBytes (.toPath f)))))

(deftest write-file-across-controlled-runtimes
  (let [out (fx/tmp-dir! "grammar-perl-wf-")
        mk (fn [id shell ks] (let [r (sh-rt id shell (bin-dir! ks))] [r (discover r)]))
        rows {:dash-perl    (mk :dash-perl :dash [:perl])
              :rbash-perl   (mk :rbash-perl :rbash [:perl])
              :rbash-both   (mk :rbash-both :rbash [:perl :python3])
              :rbash-python (mk :rbash-python :rbash [:python3])
              :rbash-none   (mk :rbash-none :rbash [])}
        write (fn [id prof c]
                (let [[r] (rows id)
                      path (str out "/" (name id) "-" (hash prof) " x.txt")
                      res (run-intent r prof [:write-file {:path path :content c}])]
                  (assoc res :path path)))
        exact? (fn [res c] (Arrays/equals (.getBytes ^String c "UTF-8") ^bytes (file-bytes (:path res))))
        results (into (sorted-map) (for [[id [_ prof]] rows] [id (write id prof content)]))
        via (fn [id] (:realization (results id)))]

    (testing "every satisfiable runtime writes the exact bytes"
      (doseq [[id res] results :when (not= id :rbash-none)]
        (testing (str id)
          (is (= :ok (:status res)))
          (is (true? (:ok? (:verified res))) (pr-str (:execution res)))
          (is (exact? res content)))))

    (testing "observed restrictions select the realization"
      (is (= :sh.write-file/redirect (first (via :dash-perl))) "sh redirect is cheapest when allowed")
      (is (= [:ctx/sh->perl :pl.write-file/open] (take 2 (via :rbash-perl))) "bash -r refuses >, perl present")
      (is (= [:ctx/sh->perl :pl.write-file/open] (take 2 (via :rbash-both))) "equal cost, ids order perl before python")
      (is (= [:ctx/sh->python :py.write-file/pathlib] (take 2 (via :rbash-python))) "perl absent: python"))

    (testing "sysopen route when 3-arg open is declared unavailable"
      (let [[_ prof] (rows :rbash-perl)
            res (write :rbash-perl (p/declare-cap prof :pl/open-write :no "test: policy forbids open") content)]
        (is (= [:ctx/sh->perl :pl.write-file/sysopen] (take 2 (:realization res))))
        (is (exact? res content))))

    (testing "forbidden single quote: perl program inside sh double quotes, perl-side literals unquoted by '"
      (let [[_ prof] (rows :rbash-perl)
            res (write :rbash-perl (p/declare-text prof :forbid #{"'"} "test: no single quotes") content)]
        (is (= :ok (:status res)))
        (is (= :ctx/sh->perl (first (:realization res))))
        (is (not (str/includes? (:code res) "'")))
        (is (exact? res content))))

    (testing "NUL in content through sh -> perl"
      (let [[_ prof] (rows :rbash-perl)
            res (write :rbash-perl prof "a\u0000b\n")]
        (is (= [:ctx/sh->perl :pl.write-file/open] (take 2 (:realization res))))
        (is (exact? res "a\u0000b\n"))))

    (testing "nothing works: unsatisfiable, perl refusal traced to its probe"
      (let [res (results :rbash-none)
            [_ prof] (rows :rbash-none)
            ex (into {} (map (juxt :id identity)) (:explain res))]
        (is (= :unsatisfiable (:status res)))
        (is (= [:interp/perl] (:no (ex :ctx/sh->perl))))
        (is (= [:interp/python3] (:no (ex :ctx/sh->python))))
        (is (= [:sh/redirect] (:no (ex :sh.write-file/redirect))))
        (is (int? (get-in prof [:evidence :ev.rbash-none/interp.perl :exit])))
        (is (some #{:pl/open-write} (:unknowns res)) "skipped perl probe stays an unknown")
        (is (some #{:ev.rbash-none/interp.perl} (:evidence res)))))

    (testing "both quotes forbidden on sh: perl present but no sh word can carry the program"
      (let [[_ prof] (rows :rbash-perl)
            res (write :rbash-perl (p/declare-text prof :forbid #{"'" "\""} "test: no quotes") content)]
        (is (= :unsatisfiable (:status res)))
        (is (pos? (:searched res)))
        (is (seq (:rejected res)))
        (is (every? (fn [{:keys [violations]}] (every? #(= :forbid (:constraint %)) violations)) (:rejected res)))
        (is (some #(= :ctx/sh->perl (first (:via %))) (:rejected res)))
        (is (not (.exists (File. ^String (:path res)))) "nothing ran")))

    (testing "evidence ids resolve in their profile"
      (doseq [[id res] results]
        (testing (str id)
          (is (seq (:evidence res)))
          (is (evidence-linked? (second (rows id)) res)))))))

(deftest write-file-on-a-perl-runtime-without-quotes
  (let [out (fx/tmp-dir! "grammar-perl-rt-")
        prof (p/declare-text (:dash @profiles) :forbid #{"'" "\""} "test: no quotes at all")
        path (str out "/no quotes \"here\".txt")
        res (run-intent (:perl @rts) prof [:write-file {:path path :content (str content "\u0000")}])]
    (is (= :ok (:status res)))
    (is (= [:pl.write-file/open :pl.lit/chr :pl.lit/chr :pl.lit/chr] (:realization res)))
    (is (not-any? #(str/includes? (:code res) %) ["'" "\""]))
    (is (true? (:ok? (:verified res))) (pr-str (:execution res)))
    (is (Arrays/equals (.getBytes (str content "\u0000") "UTF-8") ^bytes (file-bytes path)))))
