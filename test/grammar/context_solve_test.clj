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

(def v1 "Path key of the first hop." (:context (hops 0)))
(def v2 "Path key of vm1 then vm2." (:context (hops 1)))
(def root! "Path key of vm1, vm2, then sudo." (:context (hops 2)))

(defn env [profiles]
  (solve/make-env (prof :root (conj posix :reach/vm1) #{:interp/python3})
                  {:realizations (:realizations (g/compose g/modules))
                   :wrappers (into (:wrappers (g/compose g/modules)) hops)
                   :profiles profiles}))

(def far
  {v1 (prof :ssh.vm1 (conj posix :reach/vm2) #{})
   v2 (prof :ssh.vm2 (conj posix :reach/root :interp/python3 :py/sys :py/stdout-buffer) #{})
   root! (prof :sudo.root (conj posix :interp/python3 :py/sys :py/stdout-buffer) #{})})

(def world {:hosts {"local" {:login :posix} "vm1" {:login :posix} "vm2" {:login :posix}}})

(deftest the-target-picks-where-the-leaf-runs
  (doseq [[target hops-expected] [[nil []] [v1 [v1]] [v2 [v1 v2]]
                                  [root! [v1 v2 root!]]]]
    (let [res (solve/solve (env far) :sh [:str "x"] {:target target})]
      (is (= :ok (:status res)) (str target))
      (is (= hops-expected (:hops res)) (str target)))))

(deftest inner-caps-are-checked-on-the-far-side
  (testing "python3 is :no at the root but :yes on vm2: delegation to python happens only there"
    (let [res (solve/solve (env far) :sh [:str "x"] {:target v2 :stack nil})
          py (solve/solve (env (assoc far v2 (prof :ssh.vm2 (conj posix :reach/root :interp/python3 :py/sys :py/stdout-buffer) #{:sh/printf})))
                          :sh [:str "x"] {:target v2})]
      (is (= :ok (:status res)))
      (is (= :ok (:status py)))
      (is (some #{:ctx/sh->python} (:realization py)) "printf refuted on vm2, python3 proven there"))))

(deftest a-far-side-with-no-profile-is-unknown
  (let [res (solve/solve (env (dissoc far v2)) :sh [:str "x"] {:target root!})]
    (is (= :blocked-on-unknown (:status res)))
    (is (contains? (:needs-at res) v2))
    (is (empty? (:needs res)) "nothing is unknown at the root")))

(deftest a-csh-login-refutes-the-direct-hop
  (let [res (solve/solve (env (assoc far v1 (prof :ssh.vm1 (conj (disj posix :sh/posix-words) :reach/vm2) #{:sh/posix-words})))
                         :sh [:str "x"] {:target v1})]
    (is (= :unsatisfiable (:status res)))))

(deftest contexts-are-keyed-by-hop-path
  (testing "a stdin hop and a direct hop to one host have separate profiles"
    (let [[direct] (cx/chain [(cx/ssh-hop {:route :c :dest "c"})])
          [stdin] (cx/chain [(cx/ssh-stdin-hop {:route :c :dest "c"})])
          root (prof :root (conj posix :reach/c) #{})
          ;; csh behind the hop: the stdin profile proves posix words of the
          ;; sh it starts; the direct hop's own profile refutes them
          stdin-prof (prof :c posix #{})
          e (solve/make-env root {:realizations (:realizations (g/compose g/modules))
                                  :wrappers (into (:wrappers (g/compose g/modules)) [direct stdin])
                                  :profiles {(:context stdin) stdin-prof}})]
      (is (not= (:context direct) (:context stdin)))
      (is (= :unsatisfiable (:status (solve/solve (assoc-in e [:profiles (:context direct)]
                                                             (prof :c (disj posix :sh/posix-words) #{:sh/posix-words}))
                                                   :sh [:str "x"] {:target (:context direct)}))))
      (is (= :blocked-on-unknown (:status (solve/solve e :sh [:str "x"] {:target (:context direct)})))
          "the stdin hop's profile never answers for the direct hop")
      (is (= :ok (:status (solve/solve e :sh [:str "x"] {:target (:context stdin)}))))))
  (testing "sudo behind two hosts: two contexts"
    (let [a (cx/chain [(cx/ssh-hop {:route :a :dest "a"}) (cx/sudo-hop {})])
          b (cx/chain [(cx/ssh-hop {:route :b :dest "b"}) (cx/sudo-hop {})])]
      (is (not= (:context (last a)) (:context (last b))))))
  (testing "make-env refuses two wrappers with one context"
    (is (thrown? clojure.lang.ExceptionInfo
                 (solve/make-env (prof :root #{} #{}) {:wrappers [(cx/sudo-hop {}) (cx/sudo-hop {})]})))))

(deftest far-side-text-constraints-bind-the-inner-program
  (testing ":forbid on vm2 applies to what runs on vm2, not only to the outer text"
    (let [res (solve/solve (env (update far v2 p/declare-text :forbid #{"'"} "test: vm2 bans single quotes"))
                           :sh [:str "it's"] {:target v2})
          inner (-> (r/run world :sh (:code res)) :trace last :input)]
      (is (= :ok (:status res)))
      (is (not (clojure.string/includes? inner "'")) inner)
      (is (= "it's" (r/out-string (r/run world :sh (:code res)))))))
  (testing ":max-len on vm2 no inner program can meet is unsatisfiable"
    (is (= :unsatisfiable (:status (solve/solve (env (update far v2 p/declare-text :max-len 4 "test"))
                                                :sh [:str "x"] {:target v2}))))))

(defspec planned-relays-emit-the-payload-in-the-oracle 150
  (prop/for-all [s rm/gen-bytes
                 target (gen/elements [nil v1 v2 root!])]
    (let [s (.replace (String. ^bytes s "UTF-8") "\u0000" "")
          res (solve/solve (env far) :sh [:str s] {:target target})
          run (r/run world :sh (:code res))]
      (and (= :ok (:status res))
           (= s (r/out-string run))
           (= (inc (count (:hops res))) (count (:trace run)))
           (= 1 (:searched res))))))
