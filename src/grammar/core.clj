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

(defn effect
  "Observe, with no run, whether the state that intent describes holds now:
  {:check kw :holds? bool}. Defined for state intents (:write-file,
  :ensure-dir, :remove-file); nil for :str and :run, whose effect exists only
  as the output of a run. The observation reads this JVM's filesystem, so it
  is only meaningful for a runtime that shares it."
  [[op arg :as _intent]]
  (case op
    :write-file (let [f (File. ^String (:path arg))
                      actual (when (.isFile f) (Files/readAllBytes (.toPath f)))]
                  {:check :file-bytes
                   :holds? (boolean (and (some? actual)
                                         (Arrays/equals (utf8 (:content arg)) ^bytes actual)))
                   :bytes (when actual (alength ^bytes actual))})
    :ensure-dir {:check :is-directory :holds? (.isDirectory (File. ^String (:path arg)))}
    :remove-file {:check :file-absent :holds? (not (.isFile (File. ^String (:path arg))))}
    nil))

(defn verify
  "Check the observable effect of intent against what the run produced. A
  state intent verifies when the run exited 0 and its effect holds after it."
  [[op arg :as intent] run]
  (case op
    :str (let [ok (and (= 0 (:exit run))
                       (some? (:out-bytes run))
                       (Arrays/equals (utf8 arg) ^bytes (:out-bytes run)))]
           {:check :stdout-bytes :exit (:exit run) :ok? ok})
    :run {:check :exit-zero :exit (:exit run) :ok? (= 0 (:exit run))}
    (let [{:keys [holds?] :as e} (effect intent)]
      (-> (dissoc e :holds?)
          (assoc :exit (:exit run) :ok? (and (= 0 (:exit run)) holds?))))))

(defn resolve!
  "Probe, in runtime, every cap in caps that is still :unknown and has an
  on-demand probe (e.g. :bin/git). Returns the enriched profile."
  [runtime profile caps]
  (probe/resolve-caps profile (rt/runtime-id runtime) (rt/runtime-lang runtime)
                      #(rt/execute runtime %) caps))

(defn plan-resolving!
  "Plan intent for runtime; when the plan is :blocked-on-unknown, probe its
  :needs on demand and plan again, until it stops changing.
  Returns {:result Result :profile profile-after}."
  ([runtime profile intent] (plan-resolving! runtime profile intent {}))
  ([runtime profile intent opts]
   (loop [prof profile]
     (let [res (plan prof (rt/runtime-lang runtime) intent opts)
           prof' (if (= :blocked-on-unknown (:status res))
                   (resolve! runtime prof (:needs res))
                   prof)]
       (if (= prof' prof)
         {:result res :profile prof}
         (recur prof'))))))

(defn realize!
  "Plan intent for runtime's language under profile, execute it, verify it.
  A non-:ok plan is returned unchanged: nothing is run. opts :resolve true
  probes blocked unknowns on demand first (see plan-resolving!); the
  enriched profile is then under :profile-after."
  ([runtime profile intent] (realize! runtime profile intent {}))
  ([runtime profile intent opts]
   (let [{res :result prof :profile} (if (:resolve opts)
                                       (plan-resolving! runtime profile intent (dissoc opts :resolve))
                                       {:result (plan profile (rt/runtime-lang runtime) intent opts)
                                        :profile profile})
         res (cond-> (assoc res :runtime (rt/runtime-id runtime))
               (:resolve opts) (assoc :profile-after prof))]
     (if (= :ok (:status res))
       (let [run (rt/execute runtime (:code res))]
         (assoc res
                :execution (select-keys run [:exit :out :err :error])
                :verified (verify intent run)))
       res))))
