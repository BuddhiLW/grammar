;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.context-solve-test
  "The profile is per layer. Inside a hop the solver checks the inner
  program against the FAR side's profile; a far side with no profile
  makes its needs :unknown; :target says where the leaf must run; the
  planned text, run through the pure relay oracle, emits the payload."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [grammar.context :as cx]
            [grammar.core :as g]
            [grammar.parse.relay :as r]
            [grammar.profile :as p]
            [grammar.relay-model-test :as rm]
            [grammar.solve :as solve]))

(defn prof
  "A declared profile: every cap in yes is :yes, every cap in no is :no."
  [id yes no]
  (as-> (p/empty-profile id) pr
    (reduce #(p/declare-cap %1 %2 :yes "test") pr yes)
    (reduce #(p/declare-cap %1 %2 :no "test") pr no)))

(def posix #{:sh/printf :sh/single-quote :sh/double-quote :sh/posix-words :interp/sh :sh/pipe})

(def hops
  (cx/chain [(cx/ssh-hop {:route :vm1 :dest "vm1"})
             (cx/ssh-hop {:route :vm2 :dest "vm2"})
             (cx/sudo-hop {})]))

(defn env [profiles]
  (solve/make-env (prof :root (conj posix :reach/vm1) #{:interp/python3})
                  {:realizations (:realizations (g/compose g/modules))
                   :wrappers (into (:wrappers (g/compose g/modules)) hops)
                   :profiles profiles}))

(def far
  {:ssh/vm1 (prof :ssh.vm1 (conj posix :reach/vm2) #{})
   :ssh/vm2 (prof :ssh.vm2 (conj posix :reach/root :interp/python3 :py/sys :py/stdout-buffer) #{})
   :sudo/root (prof :sudo.root (conj posix :interp/python3 :py/sys :py/stdout-buffer) #{})})

(def world {:hosts {"local" {:login :posix} "vm1" {:login :posix} "vm2" {:login :posix}}})

(deftest the-target-picks-where-the-leaf-runs
  (doseq [[target hops-expected] [[nil []] [:ssh/vm1 [:ssh/vm1]] [:ssh/vm2 [:ssh/vm1 :ssh/vm2]]
                                  [:sudo/root [:ssh/vm1 :ssh/vm2 :sudo/root]]]]
    (let [res (solve/solve (env far) :sh [:str "x"] {:target target})]
      (is (= :ok (:status res)) (str target))
      (is (= hops-expected (:hops res)) (str target)))))

(deftest inner-caps-are-checked-on-the-far-side
  (testing "python3 is :no at the root but :yes on vm2: delegation to python happens only there"
    (let [res (solve/solve (env far) :sh [:str "x"] {:target :ssh/vm2 :stack nil})
          py (solve/solve (env (assoc far :ssh/vm2 (prof :ssh.vm2 (conj posix :reach/root :interp/python3 :py/sys :py/stdout-buffer) #{:sh/printf})))
                          :sh [:str "x"] {:target :ssh/vm2})]
      (is (= :ok (:status res)))
      (is (= :ok (:status py)))
      (is (some #{:ctx/sh->python} (:realization py)) "printf refuted on vm2, python3 proven there"))))

(deftest a-far-side-with-no-profile-is-unknown
  (let [res (solve/solve (env (dissoc far :ssh/vm2)) :sh [:str "x"] {:target :sudo/root})]
    (is (= :blocked-on-unknown (:status res)))
    (is (contains? (:needs-at res) :ssh/vm2))
    (is (empty? (:needs res)) "nothing is unknown at the root")))

(deftest a-csh-login-refutes-the-direct-hop
  (let [res (solve/solve (env (assoc far :ssh/vm1 (prof :ssh.vm1 (conj (disj posix :sh/posix-words) :reach/vm2) #{:sh/posix-words})))
                         :sh [:str "x"] {:target :ssh/vm1})]
    (is (= :unsatisfiable (:status res)))))

(defspec planned-relays-emit-the-payload-in-the-oracle 150
  (prop/for-all [s rm/gen-bytes
                 target (gen/elements [nil :ssh/vm1 :ssh/vm2 :sudo/root])]
    (let [s (.replace (String. ^bytes s "UTF-8") "\u0000" "")
          res (solve/solve (env far) :sh [:str s] {:target target})
          run (r/run world :sh (:code res))]
      (and (= :ok (:status res))
           (= s (r/out-string run))
           (= (inc (count (:hops res))) (count (:trace run)))
           (= 1 (:searched res))))))
