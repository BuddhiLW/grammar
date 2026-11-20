;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.runtime
  "Boundary: the only namespace that starts processes.

  A Runtime is a port: something that can execute program text in one
  language and report what happened. ProcessRuntime is the adapter for a
  real interpreter: an argv prefix (e.g. [\"/usr/bin/bash\" \"-r\" \"-c\"])
  and a complete environment. The program is passed as the last argv
  element, never through a shell, so the planner's text arrives byte for
  byte."
  (:import [java.io File InputStream]
           [java.util.concurrent TimeUnit]))

(defprotocol IRuntime
  (runtime-id [rt] "Keyword naming this runtime; evidence ids derive from it.")
  (runtime-lang [rt] "Language of the program text it accepts (:sh or :python).")
  (execute [rt code]
    "Run program text. {:argv :exit :out :out-bytes :err} when it ran;
    {:argv :error} when it never ran or did not finish."))

(defn- slurp-bytes [^InputStream is] (future (.readAllBytes is)))

(defrecord ProcessRuntime [id lang argv env timeout-ms]
  IRuntime
  (runtime-id [_] id)
  (runtime-lang [_] lang)
  (execute [_ code]
    (let [full (conj (vec argv) code)
          pb (ProcessBuilder. ^java.util.List full)]
      (.redirectInput pb (File. "/dev/null"))
      (doto (.environment pb) (.clear) (.putAll ^java.util.Map env))
      (try
        (let [proc (.start pb)
              out (slurp-bytes (.getInputStream proc))
              err (slurp-bytes (.getErrorStream proc))]
          (if (.waitFor proc timeout-ms TimeUnit/MILLISECONDS)
            (let [^bytes ob @out]
              {:argv full
               :exit (.exitValue proc)
               :out-bytes ob
               :out (String. ob "UTF-8")
               :err (String. ^bytes @err "UTF-8")})
            (do (.destroyForcibly proc)
                {:argv full :error (str "timeout after " timeout-ms "ms")})))
        (catch java.io.IOException e
          {:argv full :error (.getMessage e)})))))

(defn process-runtime
  "opts: :id :argv :env (complete; nothing is inherited), optional :lang
  (default :sh) and :timeout-ms (default 10000)."
  [{:keys [id lang argv env timeout-ms] :or {lang :sh timeout-ms 10000}}]
  (->ProcessRuntime id lang (vec argv) env timeout-ms))
