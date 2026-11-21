;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.modules-test
  "The language seam. Modules compose into one registry, in order, and a
  language added only as data probes, plans, runs and verifies like a
  built-in one."
  (:require [clojure.test :refer [deftest is testing]]
            [grammar.candidate :as c]
            [grammar.core :as core]
            [grammar.fixtures :as fx]
            [grammar.lit :as lit]
            [grammar.probe :as probe]
            [grammar.profile :as p]
            [grammar.realize :as realize]
            [grammar.schema :as schema]))

(def toy
  "A language that exists only as this map. :toy program text is POSIX sh,
  run from sh by `sh -c`; its one capability, :toy/printf, is proved by its
  own probe, which needs capabilities only :core proves."
  {:id :toy
   :realizations [{:id :toy.lit/single :intent :lit :lang :toy :cost 1 :requires #{}
                   :render (fn [_ [_ s]] (c/leaf (lit/sh-single s)))}
                  {:id :toy.str/printf :intent :str :lang :toy :cost 1 :requires #{:toy/printf}
                   :render (fn [env [_ s]] (c/combine #(str "printf %s " %) ((:lit env) :toy s)))}]
   :wrappers [{:id :ctx/sh->toy :outer :sh :inner :toy :cost 10 :requires #{:interp/sh}
               :render (fn [env code] (c/combine #(str "sh -c " %) ((:lit env) :sh code)))}]
   :probes [{:cap :toy/printf :needs [:interp/sh :sh/single-quote]
             :script "sh -c 'printf %s ok'" :expect "ok"}]})

(def with-toy (conj core/modules toy))

(deftest default-modules-are-the-built-in-registry
  (is (= [core/core-module] core/modules))
  (is (= {:realizations realize/realizations :wrappers realize/wrappers :probes probe/probes}
         (core/compose core/modules))))

(deftest colliding-modules-are-refused
  (let [e (try (core/compose [core/core-module core/core-module]) nil
               (catch clojure.lang.ExceptionInfo e e))]
    (is (some? e))
    (is (= #{:realizations :wrappers :probes} (set (keys (:duplicates (ex-data e))))))
    (is (= [:core :core] (:modules (ex-data e))))))

(deftest a-language-added-as-data
  (let [rt (fx/sh-runtime! :toy-host :dash [:sh])
        prof (core/discover! rt {:modules with-toy})
        opts {:modules with-toy :stack [:sh :toy]}]
    (testing "its probe runs after :core, which proves what it needs"
      (is (= :yes (p/verdict prof :toy/printf))))
    (testing "probe order is module order: before :core the probe is skipped, so the cap stays unknown"
      (is (= :unknown (p/verdict (core/discover! rt {:modules [toy core/core-module]}) :toy/printf))))
    (testing "without the module the capability is never probed"
      (is (= :unknown (p/verdict (core/discover! rt) :toy/printf))))
    (testing "an open Lang: a Result whose stack holds :toy validates"
      (let [res (core/plan prof :sh [:str "x"] opts)]
        (is (= :ok (:status res)))
        (is (= [:sh :toy] (:stack res)))
        (is (schema/result? res) (pr-str (schema/explain-result res)))))
    (testing "without the module that stack cannot be planned"
      (is (not= :ok (:status (core/plan prof :sh [:str "x"] {:stack [:sh :toy]})))))
    (testing "it runs for real and verifies, nasty strings included"
      (doseq [s ["" "it's $HOME `id` \\ \"q\" %s %%" "ünï\ncode\ttab"]]
        (let [res (core/realize! rt prof [:str s] opts)]
          (is (= :ok (:status res)) (pr-str s))
          (is (every? (set (:realization res)) [:ctx/sh->toy :toy.str/printf]) (pr-str (:realization res)))
          (is (true? (:ok? (:verified res))) (pr-str s (:execution res))))))))
