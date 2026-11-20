;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.core
  "Facade: discover a runtime, plan an intent, run it there, verify the effect.

    (def rt (runtime/process-runtime {:id :rbash :argv [\"/usr/bin/bash\" \"-r\" \"-c\"] :env {...}}))
    (def prof (discover! rt))
    (plan prof :sh [:write-file {:path \"/tmp/x\" :content \"hi\"}])
    (realize! rt prof [:write-file {:path \"/tmp/x\" :content \"hi\"}])"
  (:require [grammar.probe :as probe]
            [grammar.realize :as realize]
            [grammar.runtime :as rt]
            [grammar.solve :as solve])
  (:import [java.io File]
           [java.nio.file Files]
           [java.util Arrays]))

(defn discover!
  "Probe runtime and return its evidence-backed profile."
  [runtime]
  (probe/discover (rt/runtime-id runtime) #(rt/execute runtime %)))

(defn env
  "Planning environment with the built-in registry. opts pass through to
  grammar.solve/make-env (e.g. :admit)."
  ([profile] (env profile {}))
  ([profile opts]
   (solve/make-env profile (merge {:realizations realize/realizations
                                   :wrappers realize/wrappers}
                                  opts))))

(defn plan
  "Pure: choose a realization of intent in lang under profile."
  ([profile lang intent] (plan profile lang intent {}))
  ([profile lang intent opts]
   (solve/solve (env profile (select-keys opts [:admit])) lang intent (dissoc opts :admit))))

(defn- utf8 ^bytes [^String s] (.getBytes s "UTF-8"))

(defn verify
  "Check the observable effect of intent against what the run produced."
  [[op arg :as _intent] run]
  (case op
    :str (let [ok (and (= 0 (:exit run))
                       (some? (:out-bytes run))
                       (Arrays/equals (utf8 arg) ^bytes (:out-bytes run)))]
           {:check :stdout-bytes :exit (:exit run) :ok? ok})
    :write-file (let [f (File. ^String (:path arg))
                      actual (when (.isFile f) (Files/readAllBytes (.toPath f)))
                      ok (and (= 0 (:exit run))
                              (some? actual)
                              (Arrays/equals (utf8 (:content arg)) ^bytes actual))]
                  {:check :file-bytes :exit (:exit run) :ok? ok
                   :bytes (when actual (alength ^bytes actual))})))

(defn realize!
  "Plan intent for runtime's language under profile, execute it, verify it.
  A non-:ok plan is returned unchanged: nothing is run."
  ([runtime profile intent] (realize! runtime profile intent {}))
  ([runtime profile intent opts]
   (let [res (plan profile (rt/runtime-lang runtime) intent opts)]
     (if (= :ok (:status res))
       (let [run (rt/execute runtime (:code res))]
         (assoc res
                :runtime (rt/runtime-id runtime)
                :execution (select-keys run [:exit :err :error])
                :verified (verify intent run)))
       (assoc res :runtime (rt/runtime-id runtime))))))
