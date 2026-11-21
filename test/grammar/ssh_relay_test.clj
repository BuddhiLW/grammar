;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.ssh-relay-test
  "Real relay chains over ssh, gated on GRAMMAR_SSHD=1 (docker, the image
  of test/lab/sshd built as grammar-relay-lab:1). Without the gate every
  test prints a note and passes: the default suite stays offline.

  Lab: n1 published on 127.0.0.1, n2 and n3 only inside the docker
  network. relay has login sh and passwordless sudo; cshu logs in to
  tcsh, fishu to fish. Every chain is discovered THROUGH its hops, then a
  random payload is planned for the far side, run, and compared byte for
  byte."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [grammar.context :as cx]
            [grammar.core :as g]
            [grammar.fixtures :as fx]
            [grammar.lab :as lab]
            [grammar.profile :as p]
            [grammar.roundtrip-test :as rtt]
            [grammar.runtime :as rt]))

(def enabled? (= "1" (System/getenv "GRAMMAR_SSHD")))

(def ^:dynamic *lab* nil)

(use-fixtures :once
  (fn [t]
    (if-not enabled?
      (println "grammar.ssh-relay-test: skipped (set GRAMMAR_SSHD=1 and build test/lab/sshd as" lab/image ")")
      (let [L (lab/start-docker!)]
        (try
          (when-not (lab/wait-ready! L 40) (throw (ex-info "lab sshd did not answer" {})))
          (binding [*lab* L] (t))
          (finally (lab/stop! L)))))))

(def mods (conj g/modules cx/module))

(defn chains
  "Named hop chains, outermost first, over lab config cfg."
  [cfg]
  (let [o ["-F" cfg]]
    {:ssh            [(cx/ssh-hop {:route :n1 :dest "n1" :opts o})]
     :ssh-sudo       [(cx/ssh-hop {:route :n1 :dest "n1" :opts o}) (cx/sudo-hop {})]
     :ssh-ssh-nested [(cx/ssh-hop {:route :n1 :dest "n1" :opts o}) (cx/ssh-hop {:route :n2 :dest "n2"})]
     :ssh-jump       [(cx/ssh-hop {:route :n2j :dest "relay@n2" :opts o :jump "n1"})]
     :jump2-sudo-u   [(cx/ssh-hop {:route :n3j :dest "relay@n3" :opts o :jump "n1,relay@n2"})
                      (cx/sudo-hop {:route :app :user "app"})]
     :nested3-sudo   [(cx/ssh-hop {:route :n1 :dest "n1" :opts o}) (cx/ssh-hop {:route :n2 :dest "n2"})
                      (cx/ssh-hop {:route :n3 :dest "n3"}) (cx/sudo-hop {})]
     :csh-direct     [(cx/ssh-hop {:route :n2c :dest "cshu@n2" :opts o :jump "n1"})]
     :fish-direct    [(cx/ssh-hop {:route :n1 :dest "n1" :opts o}) (cx/ssh-hop {:route :n3f :dest "fishu@n3"})]
     :csh-stdin      [(cx/ssh-stdin-hop {:route :n2c :dest "cshu@n2" :opts o :jump "n1"})]
     :fish-stdin     [(cx/ssh-hop {:route :n1 :dest "n1" :opts o})
                      (cx/ssh-stdin-hop {:route :n3f :dest "fishu@n3"})]}))

(def ^:private state (atom {}))

(defn far!
  "[runtime profile] at the end of chain k, discovered once per lab."
  [k]
  (or (get @state k)
      (let [root (fx/sh-runtime! :root :dash [:sh :ssh])
            rt (cx/chain-runtime root (cx/chain (get (chains (:config *lab*)) k)))
            v [rt (g/discover! rt {:modules mods})]]
        (swap! state assoc k v)
        v)))

(defn- trial [k s stack]
  (let [[far prof] (far! k)]
    (g/realize! far prof [:str s] (if stack {:stack stack} {}))))

(deftest the-posix-words-probe-sorts-login-shells
  (when *lab*
    (doseq [[k v] {:ssh :yes :ssh-jump :yes :csh-direct :no :fish-direct :no :csh-stdin :yes :fish-stdin :yes}]
      (is (= v (p/verdict (second (far! k)) :sh/posix-words)) (name k)))))

(deftest raw-payloads-and-long-payloads-cross
  (when *lab*
    (testing "payload bytes ride inside the quoted words, not as octal: printf %s and python"
      (doseq [k [:ssh-sudo :nested3-sudo :csh-stdin :fish-stdin]
              s ["a\\\\b" "x\\';id;\\'" "'\"$`\\\n!*?~λ🚀" "-n"]]
        (let [[far prof] (far! k)
              prof (reduce #(p/declare-cap %1 %2 :no "test: raw encoders only") prof [:sh/double-quote])
              res (g/realize! far prof [:str s])]
          (is (= [:sh.str/printf :sh.lit/single] (:realization res)) (pr-str k s))
          (is (true? (:ok? (:verified res))) (pr-str k s)))))
    (testing "4-64 KB payloads"
      ;; A ' costs 4 bytes in a single-quoted word, and each of those 4
      ;; bytes holds a ' again one hop out: quote-heavy text grows about
      ;; 4x per hop. Measured program bytes: 4 KB of quote-heavy text is
      ;; 47 KB after two hops and 381 KB after four (past Linux's 128 KB
      ;; MAX_ARG_STRLEN, exec fails, the run is an :error); 64 KB of
      ;; quote-free text is 83 KB after four. The planner does not weigh
      ;; argv limits yet, so the sizes here stay under it.
      (doseq [[k s] [[:nested3-sudo (apply str (take 65536 (cycle "abcdefgh λ🚀\n-%\\")))]
                     [:ssh-ssh-nested (apply str (take 4096 (cycle "a'b\"$`\\\nλ🚀 ")))]]]
        (let [n (count s)
              res (trial k s nil)]
          (println "relay long payload" n "chars, program bytes" (get-in res [:constraints :bytes]))
          (is (true? (:ok? (:verified res))) (str n)))))))

(deftest chains-deliver-bytes-exactly
  (when *lab*
    (doseq [k [:ssh :ssh-sudo :ssh-ssh-nested :ssh-jump :jump2-sudo-u :nested3-sudo :csh-stdin :fish-stdin]]
      (testing (name k)
        (let [r (tc/quick-check 25 (prop/for-all [s rtt/gen-text]
                                     (let [res (trial k s nil)]
                                       (and (= :ok (:status res)) (true? (:ok? (:verified res)))))))]
          (println "relay" (name k) "hops" (count (get (chains "x") k)) "trials" (:num-tests r) "pass" (:pass? r))
          (is (:pass? r) (pr-str (:shrunk r))))))))

(deftest python-inside-the-far-side
  (when *lab*
    (let [res (trial :ssh-sudo "a'b\"$`\\\n!λ🚀" [:sh :python])]
      (is (= [:ctx/sh->python :py.str/stdout :py.lit/single :sh.lit/single] (:realization res)))
      (is (true? (:ok? (:verified res)))))))

(deftest the-far-side-is-where-it-says
  (when *lab*
    (let [who #(:out (rt/execute (first (far! %)) "id -un"))]
      (is (= "root\n" (who :ssh-sudo)))
      (is (= "app\n" (who :jump2-sudo-u)))
      (is (= "root\n" (who :nested3-sudo))))))
