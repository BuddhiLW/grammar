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

(def frame-open "Marker printed on the far side before the program runs." "\u0001grammar-hop\u0002")
(def frame-close "Marker before the far side's exit status." "\u0001grammar-hop-exit:")

(defn- index-of-bytes
  "Index of the first occurrence of needle in hay at or after from, or -1."
  ^long [^bytes hay ^bytes needle ^long from]
  (let [n (alength needle) m (- (alength hay) n)]
    (loop [i from]
      (cond (> i m) -1
            (java.util.Arrays/equals hay i (+ i n) needle 0 n) i
            :else (recur (inc i))))))

(defn- last-index-of-bytes ^long [^bytes hay ^bytes needle]
  (let [n (alength needle)]
    (loop [i (- (alength hay) n)]
      (cond (neg? i) -1
            (java.util.Arrays/equals hay i (+ i n) needle 0 n) i
            :else (recur (dec i))))))

(defn unframe
  "Split a framed run: the far side's stdout between the markers, its exit
  status, and any bytes before the open marker (:noise: login banners,
  rc-file output). A run whose frame is missing or torn never reached the
  far side's program, or did not finish there: {:error ...}, which a probe
  judges :unknown, never :no."
  [{:keys [out-bytes] :as run}]
  (if (:error run)
    run
    (let [^bytes ob out-bytes
          o (.getBytes ^String frame-open "UTF-8")
          c (.getBytes ^String frame-close "UTF-8")
          i (index-of-bytes ob o 0)
          j (last-index-of-bytes ob c)
          tail (when (>= j 0) (String. ob (int (+ j (alength c))) (int (- (alength ob) j (alength c))) "UTF-8"))
          code (when tail (re-matches #"(\d+)\n?" tail))]
      (if (or (neg? i) (< j i) (nil? code))
        (-> run
            (dissoc :out :out-bytes)
            (assoc :error (str "hop transport failed (outer exit " (:exit run) "): no complete frame from the far side")
                   :transport-exit (:exit run)
                   :raw-out (String. ob "UTF-8")))
        (let [body (java.util.Arrays/copyOfRange ob (int (+ i (alength o))) (int j))]
          (cond-> (assoc run :exit (Long/parseLong (second code)) :transport-exit (:exit run)
                         :out-bytes body :out (String. body "UTF-8"))
            (pos? i) (assoc :noise (String. ob 0 (int i) "UTF-8"))))))))

(defn frame
  "sh program text running code with its stdout framed for unframe. code
  runs as the script of a child `sh -c` (its own word, so an exit or a
  syntax error in it cannot skip the close marker), and the marker
  carries its exit status. The frame itself uses only printf, single
  quotes and $?."
  [code]
  (let [q (str "'" (.replace ^String code "'" "'\\''") "'")]
    (str "printf '\\001grammar-hop\\002'; sh -c " q "\nprintf '\\001grammar-hop-exit:%s\\n' \"$?\"")))

(defrecord StackedRuntime [id lang outer wrap]
  IRuntime
  (runtime-id [_] id)
  (runtime-lang [_] lang)
  (execute [_ code]
    (let [framed? (= :sh lang)
          inner (if framed? (frame code) code)]
      (if-let [outer-code (wrap inner)]
        (cond-> (assoc (execute outer outer-code) :relayed outer-code)
          framed? unframe)
        {:argv [] :error "the hop cannot carry this program"}))))

(defn stacked-runtime
  "A runtime reached THROUGH another one: execute renders code with wrap
  (fn [inner-code] -> outer-code, or nil when it cannot carry it) and runs
  the result on outer. Nothing new starts processes: outer is the only
  boundary. discover! on a stacked runtime therefore probes the far side
  of the hop (the remote host, the privileged user), with evidence ids
  derived from id.

  opts: :id :lang (of the inner programs, default :sh) :outer :wrap."
  [{:keys [id lang outer wrap] :or {lang :sh}}]
  (->StackedRuntime id lang outer wrap))
