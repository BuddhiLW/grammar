;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.relay-model-test
  "Encode-then-parse is identity through N relay hops, in the pure model:
  every hop's wrapper renders the inner program, grammar.parse.relay
  re-parses it the way that hop does (local sh, ssh's argv join plus the
  remote login shell, sudo's exec, python -c, sh reading stdin), and the
  leaf must emit exactly the payload bytes. No process runs."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [grammar.candidate :as c]
            [grammar.context :as cx]
            [grammar.lit :as lit]
            [grammar.parse.relay :as r]
            [grammar.realize :as realize]))

(def world
  {:hosts {"local" {:login :posix} "vm1" {:login :posix} "vm2" {:login :posix}
           "vm3" {:login :posix} "cbox" {:login :csh} "fbox" {:login :fish}}})

(def ^:private rz (into {} (map (juxt :id identity)) realize/wrappers))

(def hops
  "Hop name -> wrapper. Destinations name hosts of `world`."
  {:sh       (rz :ctx/sh->sh)
   :py       (rz :ctx/sh->python)
   :py->sh   (rz :ctx/python->sh)
   :py->py   (rz :ctx/python->python)
   :env      (cx/env-hop {:path "/usr/bin:/bin"})
   :sudo     (cx/sudo-hop {})
   :sudo-u   (cx/sudo-hop {:route :app :user "app"})
   :ssh1     (cx/ssh-hop {:route :vm1 :dest "vm1"})
   :ssh2     (cx/ssh-hop {:route :vm2 :dest "u2@vm2" :opts ["-p" "2222"]})
   :jump3    (cx/ssh-hop {:route :vm3 :dest "vm3" :jump "vm1,vm2"})
   :stdin-c  (cx/ssh-stdin-hop {:route :cbox :dest "cbox"})
   :stdin-f  (cx/ssh-stdin-hop {:route :fbox :dest "fbox" :jump "vm1"})})

(def by-outer
  (let [ks (sort (keys hops))]
    {:sh (vec (filter #(= :sh (:outer (hops %))) ks))
     :python (vec (filter #(= :python (:outer (hops %))) ks))}))

(defn chain
  "Hop names for choice indices, starting in :sh: each index picks among
  the hops whose outer language is the current one."
  [idxs]
  (first (reduce (fn [[acc lang] i]
                   (let [opts (by-outer lang) k (opts (mod i (count opts)))]
                     [(conj acc k) (:inner (hops k))]))
                 [[] :sh] idxs)))

(defn inner-lang [ks] (if (seq ks) (:inner (hops (last ks))) :sh))

(defn lit-env [sh-enc py-enc]
  {:lit (fn [lang s] (c/leaf (case lang :sh (sh-enc s) :python (py-enc s))))})

(defn leaf
  "Innermost program emitting payload: bytes via a printf FORMAT in sh, a
  string via sys.stdout.buffer in python."
  [env lang payload]
  (case lang
    :sh (str "printf " (:code (first ((:lit env) :sh (lit/printf-format-bytes payload)))))
    :python (str "import sys;sys.stdout.buffer.write(" (:code (first ((:lit env) :python payload))) ".encode())")))

(defn encode
  "Program text for the chain ks around leaf code."
  [env ks code]
  (reduce (fn [code k] (:code (first ((:render (hops k)) env code)))) code (reverse ks)))

(defn parse-count
  "Parses the hop model predicts: the outer sh, then each hop's own."
  [ks]
  (reduce + 1 (map #(case % (:sh :py :py->sh :env :sudo :sudo-u :ssh1 :ssh2 :jump3) 1
                      :py->py 1 (:stdin-c :stdin-f) 2) ks)))

;; ---------------------------------------------------------------- generators

(def gen-bytes
  "Arbitrary bytes, NUL and invalid UTF-8 included, biased to metacharacters."
  (gen/fmap byte-array
            (gen/vector (gen/frequency [[6 (gen/choose -128 127)]
                                        [4 (gen/elements (map int "'\"$`\\\n!*?[]{}~#;&|<> -%"))]])
                        0 24)))

(def gen-chain (gen/fmap chain (gen/vector gen/nat 1 4)))

(def gen-sh-enc (gen/elements [lit/sh-single lit/sh-double]))
(def gen-py-enc (gen/elements [lit/py-single lit/py-double lit/py-chr]))

(defn sh-leaf-chain [ks] (if (= :python (inner-lang ks)) (conj ks :py->sh) ks))

;; ---------------------------------------------------------------- properties

(defspec bytes-survive-n-hops 400
  (prop/for-all [ks (gen/fmap sh-leaf-chain gen-chain)
                 bs gen-bytes
                 se gen-sh-enc
                 pe gen-py-enc]
    (let [env (lit-env se pe)
          res (r/run world :sh (encode env ks (leaf env :sh bs)))]
      (and (nil? (:error res))
           (= (vec bs) (vec (:out res)))
           (= (parse-count ks) (count (:trace res)))))))

(deftest every-depth-is-covered
  (doseq [n [1 2 3 4]]
    (testing (str n " hops")
      (let [ks (chain (range n)) bs (byte-array (map int "a'b\"$`\\\n!λ"))
            env (lit-env lit/sh-single lit/py-single)
            ks (sh-leaf-chain ks)]
        (is (= (vec bs) (vec (:out (r/run world :sh (encode env ks (leaf env :sh bs))))))
            (pr-str ks))))))

(defspec python-leaves-survive-n-hops 200
  (prop/for-all [ks (gen/such-that #(= :python (inner-lang %)) gen-chain 100)
                 s (gen/fmap #(String. ^bytes % "UTF-8") gen-bytes)
                 se gen-sh-enc
                 pe gen-py-enc]
    (let [env (lit-env se pe)
          s (.replace ^String s "\u0000" "")
          res (r/run world :sh (encode env ks (leaf env :python s)))]
      (and (nil? (:error res)) (= s (r/out-string res))))))

(def naive-ssh
  "The classic bug: `ssh host sh -c W`. ssh joins sh, -c and W with
  spaces, so the remote shell runs `sh -c <first word of W>` and the rest
  become positional parameters."
  {:outer :sh :inner :sh
   :render (fn [env code] [{:code (str "ssh -T -- vm1 sh -c " (:code (first ((:lit env) :sh code))))}])})

(deftest the-model-catches-the-classic-bugs
  (let [env (lit-env lit/sh-single lit/py-single)
        payload (byte-array (map int "a b"))
        inner (leaf env :sh payload)]
    (testing "ssh host sh -c W loses the program"
      (let [res (r/run world :sh (:code (first ((:render naive-ssh) env inner))))]
        (is (not= (vec payload) (vec (:out res))))))
    (testing "an unquoted program after ssh is word-split by the login shell"
      (let [res (r/run world :sh (str "ssh -T -- vm1 " inner))]
        (is (some? (:error res)) "the remote parse sees printf with a bare FORMAT word")))
    (testing "a direct ssh hop into csh is refused when the program holds a newline or !"
      (doseq [s ["x\ny" "x!y"]]
        (let [w (cx/ssh-hop {:route :cbox :dest "cbox"})
              res (r/run world :sh (:code (first ((:render w) env (str "printf %s " (lit/sh-single s))))))]
          (is (re-find #"csh" (str (:error res))) s))))
    (testing "the stdin hop reaches csh and fish with any payload"
      (doseq [k [:stdin-c :stdin-f]]
        (let [bs (byte-array (map int "x\ny!'\"$"))]
          (is (= (vec bs) (vec (:out (r/run world :sh (encode env [k] (leaf env :sh bs))))))))))
    (testing "the parse trace names each host and user in order"
      (is (= [["local" "me"] ["vm1" "me"] ["vm1" "root"] ["vm3" "root"]]
             (mapv (juxt :host :user) (:trace (r/run world :sh (encode env [:ssh1 :sudo :jump3] inner)))))))))

;; ---------------------------------------------------------------- raw payloads, hostile hosts

(def hostile-hops
  "Direct hops into non-POSIX login shells: the planner must never use
  them (:sh/posix-words refutes them), and the oracle must refuse what
  they would garble rather than deliver other bytes."
  {:ssh-fish (cx/ssh-hop {:route :fbox :dest "fbox"})
   :ssh-csh  (cx/ssh-hop {:route :cbox :dest "cbox"})})

(def all-hops (merge hops hostile-hops))

(def posix-probe (first (filter #(= :sh/posix-words (:cap %)) cx/probes)))

(def gen-raw-text
  "Payload text crossing hops AS RAW BYTES inside the quoted words:
  quotes, $, backticks, backslash runs, backslash-quote, newlines, !,
  non-ASCII, astral code points. No NUL (no argv word holds one)."
  (gen/fmap #(apply str %)
            (gen/vector (gen/one-of [(gen/elements ["'" "\\" "\\\\" "\\'" "\"" "$" "`" "\n" "!" " " "*" "-"
                                                    "λ" "🚀" "%s" "x"])
                                     gen/string-ascii])
                        0 16)))

(defn raw-leaf
  "printf %s W: the payload itself, one quoted word, no octal escape."
  [env s]
  (str "printf %s " (:code (first ((:lit env) :sh s)))))

(def gen-hostile-chain
  "1..4 sh hops, any of which may be a direct hop into fish or csh."
  (gen/vector (gen/elements (concat [:sh :env :sudo :ssh1 :jump3 :stdin-c] (keys hostile-hops))) 1 4))

(defn encode* [env ks code]
  (reduce (fn [code k] (:code (first ((:render (all-hops k)) env code)))) code (reverse ks)))

(def raw-outcomes (atom {:ok 0 :refused 0}))

(def raw-prop
  (prop/for-all [ks gen-hostile-chain
                 s gen-raw-text
                 se gen-sh-enc]
    (let [env (lit-env se lit/py-single)
          res (r/run world :sh (encode* env ks (raw-leaf env s)))
          hostile? (some hostile-hops ks)]
      (swap! raw-outcomes update (cond (:error res) :refused
                                       (= s (r/out-string res)) :ok
                                       :else :garbled) (fnil inc 0))
      (if hostile?
        ;; a direct hop into fish or csh may garble or refuse; what must
        ;; hold is that the planner can never pick it: the posix-words
        ;; probe sent through every hostile hop of the chain fails
        (every? (fn [k] (not= (:expect posix-probe) (r/out-string (r/run world :sh (encode* env [k] (:script posix-probe))))))
                (filter hostile-hops ks))
        (and (nil? (:error res)) (= s (r/out-string res)))))))

(deftest raw-acceptance-is-not-vacuous
  (reset! raw-outcomes {:ok 0 :refused 0})
  (let [q (clojure.test.check/quick-check 600 raw-prop)]
    (is (:pass? q) (pr-str (:shrunk q))))
  (let [{:keys [ok refused garbled] :or {garbled 0}} @raw-outcomes]
    (println "raw payloads, chains with direct fish/csh hops allowed: delivered" ok "refused" refused "garbled" garbled)
    (is (> ok 250) (str "delivered " ok " of 600"))
    (is (> refused 60) (str "refused " refused " of 600: the hostile hops must be exercised"))))
