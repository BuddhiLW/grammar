;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.vagrant-relay-test
  "host -> vm1 -> vm2 -> vm3 over real VirtualBox VMs (test/lab/vagrant),
  gated on GRAMMAR_VAGRANT=1 and GRAMMAR_VAGRANT_KEY (the private key the
  lab was provisioned with; GRAMMAR_VAGRANT_VM1_HOST/PORT point at vm1
  when the host-only network is not routable from the host, e.g. the NAT
  forward 127.0.0.1:2200). The VMs are brought up and destroyed outside
  the suite: `vagrant up` takes minutes.

  The solver plans every chain itself from discovered profiles, :target
  naming the far end; the plan runs on the host and is compared byte for
  byte. vm2's cshu logs in to tcsh: the direct hop into it must be
  refuted by :sh/posix-words, and the stdin hop must reach it."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [grammar.context :as cx]
            [grammar.core :as g]
            [grammar.fixtures :as fx]
            [grammar.profile :as p]
            [grammar.roundtrip-test :as rtt]
            [grammar.runtime :as rt]
            [grammar.solve :as solve]))

(def enabled? (and (= "1" (System/getenv "GRAMMAR_VAGRANT")) (System/getenv "GRAMMAR_VAGRANT_KEY")))

(def mods (conj g/modules cx/module))

(defn config!
  "An ssh_config for the lab in a temp file: vm1 by its private-network
  address, the rest by name from inside."
  ([] (config! (System/getenv "GRAMMAR_VAGRANT_KEY")
               (or (System/getenv "GRAMMAR_VAGRANT_VM1_HOST") "192.168.56.211")
               (or (System/getenv "GRAMMAR_VAGRANT_VM1_PORT") "22")))
  ([k host port]
   (let [d (.toFile (java.nio.file.Files/createTempDirectory "grammar-vlab-" (make-array java.nio.file.attribute.FileAttribute 0)))
         f (java.io.File. d "config")
         cp (str d "/cm-%C")]
     (.deleteOnExit d)
     (.deleteOnExit f)
     (spit f (str/join "\n" ["Host *" (str "  IdentityFile " k) "  IdentitiesOnly yes"
                             "  UserKnownHostsFile /dev/null" "  StrictHostKeyChecking no" "  LogLevel ERROR"
                             "  ConnectTimeout 15"
                             "Host vm1" (str "  HostName " host) (str "  Port " port) "  User relay"
                             "  ControlMaster auto" (str "  ControlPath " cp)
                             "  ControlPersist 120" ""]))
     (str f))))

(def chains
  "Named chains: hop constructors over ssh options o, outermost first."
  {:jump-vm3                 (fn [o] [(cx/ssh-hop {:route :vm3j :dest "relay@vm3" :opts o :jump "vm1,relay@vm2"})])
   :nested-3-sudo            (fn [o] [(cx/ssh-hop {:route :vm1 :dest "vm1" :opts o})
                                      (cx/ssh-hop {:route :vm2 :dest "relay@vm2"})
                                      (cx/ssh-hop {:route :vm3 :dest "relay@vm3"})
                                      (cx/sudo-hop {})])
   :mixed-jump-nested-sudo-u (fn [o] [(cx/ssh-hop {:route :vm2j :dest "relay@vm2" :opts o :jump "vm1"})
                                      (cx/ssh-hop {:route :vm3 :dest "relay@vm3"})
                                      (cx/sudo-hop {:route :app :user "app"})])
   :csh-direct               (fn [o] [(cx/ssh-hop {:route :vm1 :dest "vm1" :opts o})
                                      (cx/ssh-hop {:route :vm2c :dest "cshu@vm2"})])
   :fish-direct              (fn [o] [(cx/ssh-hop {:route :vm1 :dest "vm1" :opts o})
                                      (cx/ssh-hop {:route :vm2 :dest "relay@vm2"})
                                      (cx/ssh-hop {:route :vm3f :dest "fishu@vm3"})])
   :csh-stdin                (fn [o] [(cx/ssh-hop {:route :vm1 :dest "vm1" :opts o})
                                      (cx/ssh-stdin-hop {:route :vm2c :dest "cshu@vm2"})])
   :fish-stdin-3             (fn [o] [(cx/ssh-hop {:route :vm1 :dest "vm1" :opts o})
                                      (cx/ssh-hop {:route :vm2 :dest "relay@vm2"})
                                      (cx/ssh-stdin-hop {:route :vm3f :dest "fishu@vm3"})])})

(defn lab-chain
  "Discovered chain k: {:root :hops :target :profiles :runtimes :env :discovery-ms}."
  [cfg k]
  (let [root (fx/sh-runtime! :root :dash [:sh :ssh])
        ws (cx/chain ((chains k) ["-F" cfg]))
        t0 (System/currentTimeMillis)
        d (cx/discover-chain! #(g/discover! % {:modules mods}) root ws)
        reg (g/compose mods)]
    {:root root :hops ws :target (:context (last ws))
     :profiles (:profiles d) :runtimes (:runtimes d)
     :discovery-ms (- (System/currentTimeMillis) t0)
     :env (solve/make-env (:root d) {:realizations (:realizations reg)
                                     :wrappers (into (:wrappers reg) ws)
                                     :profiles (:profiles d)})}))

(defn trial [{:keys [root env target]} s]
  (let [res (solve/solve env :sh [:str s] {:target target})
        run (when (= :ok (:status res)) (rt/execute root (:code res)))]
    {:res res
     :ok? (boolean (and run (= 0 (:exit run))
                        (java.util.Arrays/equals (.getBytes ^String s "UTF-8") ^bytes (:out-bytes run))))}))

(defn check-chain [cfg k n]
  (let [lc (lab-chain cfg k)
        one (trial lc "x")
        q (tc/quick-check n (prop/for-all [s rtt/gen-text] (:ok? (trial lc s))))]
    (println "vagrant" (name k) "hops" (count (:hops lc)) "discovery-ms" (:discovery-ms lc)
             "searched" (:searched (:res one)) "planned-hops" (:hops (:res one))
             "trials" (:num-tests q) "pass" (:pass? q))
    {:lc lc :one one :q q}))

(deftest vagrant-chains
  (if-not enabled?
    (println "grammar.vagrant-relay-test: skipped (GRAMMAR_VAGRANT=1 and GRAMMAR_VAGRANT_KEY after vagrant up in test/lab/vagrant)")
    (let [cfg (config!)]
      (doseq [k [:jump-vm3 :nested-3-sudo :mixed-jump-nested-sudo-u :csh-stdin :fish-stdin-3]]
        (testing (name k)
          (let [{:keys [lc one q]} (check-chain cfg k 20)]
            (is (= (mapv :context (:hops lc)) (:hops (:res one))))
            (is (:pass? q) (pr-str (:shrunk q))))))
      (doseq [k [:csh-direct :fish-direct]]
        (testing (str (name k) ": a direct hop into a non-POSIX login is refuted; the planner never runs it")
          (let [lc (lab-chain cfg k)]
            (println "vagrant" (name k) "posix-words" (p/verdict (get (:profiles lc) (:target lc)) :sh/posix-words))
            (is (= :no (p/verdict (get (:profiles lc) (:target lc)) :sh/posix-words)))
            (is (= :unsatisfiable (:status (solve/solve (:env lc) :sh [:str "a\\\\b\\';id;\\'"] {:target (:target lc)})))))))
      (testing "the far end is who it says"
        (let [lc (lab-chain cfg :mixed-jump-nested-sudo-u)
              far (get-in lc [:runtimes (:target lc)])]
          (is (= "app\nvm3\n" (:out (rt/execute far "id -un; hostname")))))))))
