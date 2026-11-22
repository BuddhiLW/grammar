;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.resolve-test
  "plan-resolving! terminates whatever the on-demand probes do: a probe
  skipped for an unproven prerequisite, a runtime that cannot launch, a
  runtime whose execute throws, and random mixtures of them."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [grammar.core :as g]
            [grammar.profile :as p]
            [grammar.runtime :as rt]
            [grammar.schema :as s]))

(defrecord FakeRuntime [id lang f]
  rt/IRuntime
  (runtime-id [_] id)
  (runtime-lang [_] lang)
  (execute [_ code] (f code)))

(defn- answering
  "f for FakeRuntime: every script answers with outcome."
  [outcome]
  (fn [_]
    (case outcome
      :yes {:argv [] :exit 0 :out "ok" :err ""}
      :no {:argv [] :exit 0 :out "no" :err ""}
      :error {:argv [] :error "cannot launch"}
      :throw (throw (RuntimeException. "execute blew up")))))

(def ^:private test-run [:run {:argv ["test" "-d" "/"]}])

(defn- resolving
  "plan-resolving! on another thread, nil when it has not returned in 10 s."
  [& args]
  (let [f (future (apply g/plan-resolving! args))
        v (deref f 10000 nil)]
    (when-not v (future-cancel f))
    v))

(deftest skipped-probe-terminates
  (testing "no printf: discovery proves nothing, :sh.cmd/test is skipped, and it stops"
    (let [r (->FakeRuntime :noprintf :sh (answering :error))
          {res :result prof :profile :as out} (resolving r (g/discover! r) test-run)]
      (is (some? out) "plan-resolving! returned")
      (is (= :blocked-on-unknown (:status res)))
      (is (s/result? res))
      (is (= :unknown (p/verdict prof :sh.cmd/test)))
      (is (some #{:sh.cmd/test} (:unresolved out)))
      (is (<= (:passes out) 2) (pr-str (select-keys out [:passes :stopped])))
      (is (re-find #"skipped: prerequisite not proven"
                   (str (get-in prof [:evidence :ev.noprintf/sh.cmd.test :reason])))))))

(deftest throwing-runtime-terminates
  (testing "discovery passed, then execute throws: the probe is :unknown, not retried"
    (let [calls (atom 0)
          ok (->FakeRuntime :thr :sh (answering :yes))
          r (->FakeRuntime :thr :sh (fn [_] (swap! calls inc) ((answering :throw) nil)))
          out (resolving r (g/discover! ok) test-run)
          ev (get-in out [:profile :evidence :ev.thr/sh.cmd.test])]
      (is (some? out))
      (is (= :blocked-on-unknown (:status (:result out))))
      (is (= [:sh.cmd/test] (:unresolved out)))
      (is (= 1 @calls) "the probe ran once")
      (is (= :unknown (:verdict ev)))
      (is (re-find #"execute blew up" (str (:error ev))))))
  (testing "discover! survives a throwing runtime: every cap stays :unknown"
    (let [prof (g/discover! (->FakeRuntime :thr2 :sh (answering :throw)))]
      (is (s/profile? prof))
      (is (every? #(= :unknown (:verdict %)) (vals (:caps prof)))))))

(deftest answered-probes-plan
  (testing "a probe that answers resolves the plan in one more pass"
    (let [r (->FakeRuntime :yes :sh (answering :yes))
          out (resolving r (g/discover! r) test-run)]
      (is (= :ok (:status (:result out))))
      (is (= :planned (:stopped out)))
      (is (= 2 (:passes out)))
      (is (nil? (:unresolved out)))))
  (testing "a refuted probe stops with the plan unsatisfiable"
    (let [ok (->FakeRuntime :refute :sh (answering :yes))
          r (->FakeRuntime :refute :sh (answering :no))
          out (resolving r (g/discover! ok) test-run)]
      (is (= :unsatisfiable (:status (:result out))))
      (is (= :no (p/verdict (:profile out) :sh.cmd/test))))))

(deftest pass-bound-is-a-backstop
  (let [r (->FakeRuntime :bound :sh (answering :yes))
        out (resolving r (g/discover! r) test-run {:max-passes 1})]
    (is (= :pass-bound (:stopped out)))
    (is (= 1 (:passes out)))
    (is (= :blocked-on-unknown (:status (:result out))) "the last Result, unchanged")
    (is (= [:sh.cmd/test] (:unresolved out)))))

(def ^:private gen-intent
  (gen/elements [test-run
                 [:run {:argv ["git" "status"]}]
                 [:run {:argv ["mkdir" "x"]}]
                 [:write-file {:path "/tmp/x" :content "hi"}]
                 [:ensure-dir {:path "/tmp/d"}]
                 [:remove-file {:path "/tmp/x"}]
                 [:http-get {:url "http://127.0.0.1:1/"}]
                 [:str "hi"]]))

(defspec plan-resolving-terminates-for-random-outcomes 60
  (prop/for-all [lang (gen/elements [:sh :python])
                 disc (gen/vector (gen/elements [:yes :no :error]) 1 8)
                 live (gen/vector (gen/elements [:yes :no :error :throw]) 1 8)
                 intent gen-intent]
    (let [script-outcome (fn [outcomes] (fn [code] ((answering (nth outcomes (mod (hash code) (count outcomes)))) code)))
          prof (g/discover! (->FakeRuntime :rnd lang (script-outcome disc)))
          out (resolving (->FakeRuntime :rnd lang (script-outcome live)) prof intent)]
      (and (some? out)
           (s/result? (:result out))
           (s/profile? (:profile out))
           (<= 1 (:passes out) g/default-max-passes)
           (contains? #{:planned :exhausted :fixpoint :pass-bound} (:stopped out))
           (= (= :planned (:stopped out)) (not= :blocked-on-unknown (:status (:result out))))))))
