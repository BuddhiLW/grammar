;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.stacked-runtime-test
  "A StackedRuntime runs its programs through a hop on an outer runtime,
  so discovering it profiles the far side. The toy hop is env -i with a
  PATH holding only sh: the far side has no python3, the near side has."
  (:require [clojure.test :refer [deftest is testing]]
            [grammar.context :as cx]
            [grammar.core :as g]
            [grammar.fixtures :as fx]
            [grammar.profile :as p]
            [grammar.runtime :as rt]))

(def mods (conj g/modules cx/module))

(def ^:private lab
  (delay
    (let [root (fx/sh-runtime! :root :dash [:sh :python3 :env])
          hop (cx/env-hop {:path (fx/bin-dir! [:sh])})
          far (cx/hop-runtime root hop)]
      {:root root :hop hop :far far
       :p-root (g/discover! root {:modules mods})
       :p-far (g/discover! far {:modules mods})})))

(deftest discovery-through-the-hop-profiles-the-far-side
  (let [{:keys [far p-root p-far]} @lab]
    (is (= :env.env-clean (rt/runtime-id far)))
    (is (= :yes (p/verdict p-root :interp/python3)))
    (is (= :no (p/verdict p-far :interp/python3)) "env -i PATH=<only sh> hides python3")
    (is (= :yes (p/verdict p-far :sh/posix-words)))
    (is (every? #(= "ev.env.env-clean" (namespace %)) (keys (:evidence p-far))))))

(deftest a-plan-for-the-far-side-runs-through-the-hop
  (let [{:keys [far p-far]} @lab]
    (doseq [s ["" "it's $HOME `id` \\ \"q\"\n" "-n" "λ🚀 * ? [a]"]]
      (let [res (g/realize! far p-far [:str s])]
        (is (= :ok (:status res)) (pr-str s))
        (is (true? (:ok? (:verified res))) (pr-str s))))))

(deftest on-demand-probes-also-cross-the-hop
  (let [{:keys [root far p-root p-far]} @lab
        intent [:run {:argv ["python3" "-c" "pass"]}]]
    (is (= :ok (get-in (g/plan-resolving! root p-root intent) [:result :status])))
    (is (= :unsatisfiable (get-in (g/plan-resolving! far p-far intent) [:result :status]))
        "command -v python3 runs beyond env -i, where PATH holds only sh")))

(deftest a-hop-that-cannot-carry-the-program-never-runs
  (let [far (rt/stacked-runtime {:id :x :outer (:root @lab) :wrap (constantly nil)})]
    (is (some? (:error (rt/execute far "printf %s ok"))))))

(deftest a-failed-hop-is-unknown-not-refuted
  (let [root (:root @lab)
        dead (rt/stacked-runtime {:id :dead :outer root :wrap (fn [_] "echo 'Permission denied (publickey).' >&2; exit 255")})
        p (g/discover! dead {:modules mods})]
    (testing "ssh's 255, sudo -n's refusal: no frame, so :error and :unknown"
      (is (some? (:error (rt/execute dead "printf %s ok"))))
      (is (= 255 (:transport-exit (rt/execute dead "printf %s ok"))))
      (is (= :unknown (p/verdict p :sh/printf)))
      (is (empty? (filter #(= :no (:verdict %)) (vals (:evidence p))))))))

(deftest far-side-noise-and-status-are-separated
  (let [root (:root @lab)
        noisy (rt/stacked-runtime {:id :noisy :outer root
                                   :wrap #(str "printf %s 'motd: welcome'; sh -c " (grammar.lit/sh-single %))})
        run (rt/execute noisy "printf %s ok; exit 3")]
    (is (= "ok" (:out run)))
    (is (= 3 (:exit run)) "the far program's own status, not the transport's")
    (is (= "motd: welcome" (:noise run)) "rc-file output is kept for diagnosis, never judged")))
