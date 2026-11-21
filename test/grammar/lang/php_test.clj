;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.lang.php-test
  "PHP as a target, against the real php binary. The registry and the
  probe set are composed explicitly (grammar.core is not wired to PHP yet).

  Restrictions are produced for real: a `php` wrapper script in the scratch
  bin dir runs the real binary with `-d disable_functions=...`, and the
  probes observe the result through function_exists. Nothing is declared
  by hand."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [grammar.core :as g]
            [grammar.fixtures :as fx]
            [grammar.lang.php :as php]
            [grammar.probe :as probe]
            [grammar.profile :as p]
            [grammar.realize :as realize]
            [grammar.roundtrip-test :as rtt]
            [grammar.runtime :as rt]
            [grammar.solve :as solve])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

;; ---------------------------------------------------------------- composition

(def all-probes (into probe/probes php/probes))

(defn env [profile]
  (solve/make-env profile {:realizations (into realize/realizations php/realizations)
                           :wrappers (into realize/wrappers php/wrappers)}))

(defn discover [runtime]
  (probe/discover (rt/runtime-id runtime) #(rt/execute runtime %) all-probes))

(defn plan
  ([profile intent] (plan profile intent {}))
  ([profile intent opts] (solve/solve (env profile) :sh intent opts)))

(defn exec! [runtime res]
  (let [r (rt/execute runtime (:code res))]
    (assoc res :execution (select-keys r [:exit :err :error])
           :verified (g/verify (:intent res) r))))

;; ---------------------------------------------------------------- controlled runtimes

(def php-bin
  "The real php: the host PATH's, else the linuxbrew install."
  (or (fx/which "php")
      (let [f (File. "/home/linuxbrew/.linuxbrew/bin/php")] (when (.canExecute f) (.getCanonicalPath f)))))

(defn- link! [dir tool target]
  (Files/createSymbolicLink (.toPath (File. ^String dir ^String tool))
                            (.toPath (File. ^String target))
                            (make-array FileAttribute 0)))

(defn bin-dir!
  "Scratch bin dir with links for tool-ks (keys of grammar.fixtures/tools)
  and, by php-mode: nil none, :real a link to the real php, or a set of
  function names: a `php` script running the real php with exactly those
  functions disabled."
  [tool-ks php-mode]
  (let [dir (fx/tmp-dir! "grammar-php-bin-")]
    (doseq [k tool-ks] (link! dir (name k) (fx/tools k)))
    (cond
      (= :real php-mode) (link! dir "php" php-bin)
      (set? php-mode)
      (let [f (File. ^String dir "php")]
        (spit f (str "#!/usr/bin/dash\nexec " php-bin
                     " -d disable_functions=" (str/join "," (sort php-mode)) " \"$@\"\n"))
        (.setExecutable f true)))
    dir))

(defn runtime! [id shell tool-ks php-mode]
  (rt/process-runtime {:id id :lang :sh :argv (fx/shells shell)
                       :env {"PATH" (bin-dir! tool-ks php-mode) "LC_ALL" "C.UTF-8"}}))

(def ^:private rts
  (delay {:full (runtime! :php-full :dash [:sh :python3] :real)
          :php (rt/process-runtime {:id :php-r :lang :php :argv [php-bin "-r"]
                                    :env {"PATH" (bin-dir! [:sh] nil) "LC_ALL" "C.UTF-8"}})}))

(def ^:private full-profile (delay (discover (:full @rts))))

(deftest php-is-present
  (is (some? php-bin) "these tests need a real php on PATH")
  (is (= :yes (p/verdict @full-profile :interp/php)))
  (is (every? #(= :yes (p/verdict @full-profile (php/fn-cap %))) php/functions)))

;; ---------------------------------------------------------------- literal round trips

(defn- php-roundtrip? [encode s]
  (let [run (rt/execute (:php @rts) (str "fwrite(STDOUT," (encode s) ");"))]
    (true? (:ok? (g/verify [:str s] run)))))

(defspec php-single-roundtrip 100
  (prop/for-all [s rtt/gen-text] (php-roundtrip? php/php-single s)))

(defspec php-chr-roundtrip 100
  (prop/for-all [s rtt/gen-text-nul] (php-roundtrip? php/php-chr s)))

(deftest encoder-shapes
  (is (nil? (php/php-single "a\u0000b")) "an argv word cannot carry NUL")
  (is (= "'\\\\\\''" (php/php-single "\\'")))
  (is (= "'$x\n\"\t'" (php/php-single "$x\n\"\t")) "$, \", newline and tab stay literal")
  (doseq [s ["" "it's" "\"\\" "λ🚀\u0000"]]
    (is (not-any? #(str/includes? (php/php-chr s) %) ["'" "\"" "$" "`" "\\"]))))

;; ---------------------------------------------------------------- :str through stacks

(def gen-php-stack
  "[:sh :php] followed by any mix of sh, python and php layers."
  (gen/fmap #(into [:sh :php] %) (gen/vector (gen/elements [:sh :python :php]) 0 3)))

(defspec php-stacks-roundtrip 100
  (prop/for-all [stack gen-php-stack
                 s rtt/gen-text]
    (let [res (exec! (:full @rts) (plan @full-profile [:str s] {:stack stack}))]
      (and (= :ok (:status res))
           (= stack (:stack res))
           (true? (:ok? (:verified res)))))))

(deftest fixed-nasty-strings-through-php-stacks
  (doseq [stack [[:sh :php] [:sh :php :php :php] [:sh :php :sh :php] [:sh :python :php :python]
                 [:sh :sh :php :sh] [:sh :php :python :php]]
          s ["" "'" "'\\''\"$`\\" "%s%%-n\n\n" "<?php ?>" "λ🚀\u0001\u007f"]]
    (testing (pr-str stack s)
      (let [res (exec! (:full @rts) (plan @full-profile [:str s] {:stack stack}))]
        (is (= :ok (:status res)))
        (is (= stack (:stack res)))
        (is (true? (:ok? (:verified res))) (pr-str (:code res) (:execution res)))))))

(deftest str-in-php-realizations
  (testing "unrestricted: fwrite with a single-quoted literal"
    (let [res (plan @full-profile [:str "it's"] {:stack [:sh :php]})]
      (is (= [:ctx/sh->php :php.str/fwrite :php.lit/single :sh.lit/single] (:realization res)))
      (is (some #{:php.fn/fwrite} (get-in res [:constraints :requires])))))
  (testing "NUL: only chr can carry it inside php"
    (let [res (exec! (:full @rts) (plan @full-profile [:str "a\u0000b"] {:stack [:sh :php]}))]
      (is (= :php.lit/chr (nth (:realization res) 2)))
      (is (true? (:ok? (:verified res))))))
  (testing "no quote characters allowed: the chr form, through a double-quoted sh word"
    (let [prof (p/declare-text @full-profile :forbid #{"'"} "test: no single quotes")
          res (exec! (:full @rts) (plan prof [:str "it's \"x\" $HOME\n"] {:stack [:sh :php]}))]
      (is (= :ok (:status res)))
      (is (= :php.lit/chr (nth (:realization res) 2)))
      (is (not (str/includes? (:code res) "'")))
      (is (true? (:ok? (:verified res))))))
  (testing "deterministic"
    (is (= (plan @full-profile [:str "x"] {:stack [:sh :php :python]})
           (plan @full-profile [:str "x"] {:stack [:sh :php :python]})))))

;; ---------------------------------------------------------------- probes

(deftest php-probe-laws
  (let [fwrite-off (discover (runtime! :php-nofwrite :dash [:sh] #{"fwrite"}))
        no-php (discover (runtime! :php-absent :dash [:sh] nil))]
    (testing "every php probe leaves one evidence entry that agrees with its verdict"
      (doseq [prof [@full-profile fwrite-off no-php]
              {:keys [cap]} php/probes]
        (let [ids (get-in prof [:caps cap :evidence])
              ev (get-in prof [:evidence (first ids)])]
          (testing (str (:id prof) " " cap)
            (is (= 1 (count ids)))
            (is (= (:verdict ev) (p/verdict prof cap)))))))
    (testing "disable_functions is observed: function_exists answers false"
      (let [ev (get-in fwrite-off [:evidence :ev.php-nofwrite/php.fn.fwrite])]
        (is (= :no (:verdict ev)))
        (is (= "no" (:out ev)))
        (is (= 0 (:exit ev))))
      (is (= :yes (p/verdict fwrite-off :php.fn/fputs))))
    (testing "without php every php probe is skipped and stays :unknown"
      (is (= :no (p/verdict no-php :interp/php)))
      (doseq [{:keys [cap]} (rest php/probes)]
        (is (= :unknown (p/verdict no-php cap)))
        (is (str/starts-with? (get-in no-php [:evidence (probe/evidence-id :php-absent cap) :reason])
                              "skipped"))))
    (testing "fwrite disabled: :str falls back to echo, a language construct"
      (let [rtm (runtime! :php-nofwrite2 :dash [:sh] #{"fwrite"})
            res (exec! rtm (plan (discover rtm) [:str "it's\n"] {:stack [:sh :php]}))]
        (is (= :php.str/echo (second (:realization res))))
        (is (true? (:ok? (:verified res))))))))

;; ---------------------------------------------------------------- :write-file acceptance

(def content "it's \"x\" $HOME `id` 100% -n\\\n\tλ 🚀 <?php\n")

(def restrictions
  "rbash (refuses >) with only php on PATH: no tee, no sh, no python3, so
  every write must happen inside php."
  [[:php-open    #{}]
   [:php-no-fpc  #{"file_put_contents"}]
   [:php-no-fwr  #{"file_put_contents" "fwrite"}]
   [:php-no-chr  #{"chr"}]
   [:php-sealed  #{"file_put_contents" "fopen" "fwrite" "fputs"}]])

(deftest write-file-across-disable-functions
  (let [out (fx/tmp-dir! "grammar-php-wf-")
        rts (into {} (for [[id fs] restrictions] [id (runtime! id :rbash [] fs)]))
        profs (update-vals rts discover)
        write (fn [id prof c]
                (let [res (plan prof [:write-file {:path (str out "/" (name id) " " (hash c) ".txt")
                                                   :content c}])]
                  (if (= :ok (:status res)) (exec! (rts id) res) res)))
        results (into (sorted-map) (for [[id] restrictions] [id (write id (profs id) content)]))
        via (fn [id] (:realization (results id)))]

    (testing "restrictions are observed by probes, not declared"
      (doseq [[id fs] restrictions
              f fs]
        (is (= :no (p/verdict (profs id) (php/fn-cap f))) (str id " " f)))
      (doseq [[id prof] profs]
        (is (= :no (p/verdict prof :sh/redirect)) (str id))
        (is (= :yes (p/verdict prof :interp/php)) (str id))
        (is (every? #(= :probe (:kind %)) (vals (:evidence prof))) (str id))))

    (testing "every satisfiable runtime writes the exact bytes"
      (doseq [[id res] results :when (not= id :php-sealed)]
        (testing (str id)
          (is (= :ok (:status res)))
          (is (= [:php] (rest (:stack res))))
          (is (true? (:ok? (:verified res))) (pr-str (:code res) (:execution res))))))

    (testing "a different realization per restriction"
      (is (= [:ctx/sh->php :php.write-file/put-contents] (take 2 (via :php-open))))
      (is (= [:ctx/sh->php :php.write-file/fopen-fwrite] (take 2 (via :php-no-fpc))))
      (is (= [:ctx/sh->php :php.write-file/fopen-fputs] (take 2 (via :php-no-fwr))))
      (is (= 3 (count (distinct (map #(second (via %)) [:php-open :php-no-fpc :php-no-fwr]))))))

    (testing "chr disabled: the no-quote encoder is out, quotes still work"
      (is (= :no (p/verdict (profs :php-no-chr) :php.fn/chr)))
      (let [intent [:write-file {:path (str out "/no-quote-control.txt") :content "it's"}]
            forbid #(p/declare-text % :forbid #{"'"} "test: no single quotes")]
        (testing "control: with chr available the same forbid is satisfied by :php.lit/chr"
          (let [res (exec! (rts :php-open) (plan (forbid (profs :php-open)) intent))]
            (is (= :ok (:status res)))
            (is (some #{:php.lit/chr} (:realization res)))
            (is (true? (:ok? (:verified res))) (pr-str (:execution res)))))
        (is (= :unsatisfiable (:status (plan (forbid (profs :php-no-chr)) intent))))))

    (testing "NUL content rides the chr encoder inside file_put_contents"
      (let [res (write :php-open (profs :php-open) "a\u0000b\n")]
        (is (= :php.write-file/put-contents (second (:realization res))))
        (is (some #{:php.lit/chr} (:realization res)))
        (is (true? (:ok? (:verified res))))))

    (testing "every write function disabled: unsatisfiable, refusals traced to probes"
      (let [res (results :php-sealed)
            prof (profs :php-sealed)
            rows (into {} (map (juxt :id identity)) (:explain res))]
        (is (= :unsatisfiable (:status res)))
        (is (= [:php.fn/file_put_contents] (:no (rows :php.write-file/put-contents))))
        (is (= [:php.fn/fopen :php.fn/fwrite] (:no (rows :php.write-file/fopen-fwrite))))
        (is (= [:php.fn/fopen :php.fn/fputs] (:no (rows :php.write-file/fopen-fputs))))
        (is (= [:sh/redirect] (:no (rows :sh.write-file/redirect))))
        (is (= [:bin/tee] (:no (rows :sh.write-file/tee))))
        (is (= [] (:no (rows :ctx/sh->php))))
        (is (some #{:ev.php-sealed/php.fn.fopen} (:evidence res)))
        (is (= "no" (get-in prof [:evidence :ev.php-sealed/php.fn.fopen :out])))
        (is (not (.exists (File. (str out "/php-sealed " (hash content) ".txt")))))))))
