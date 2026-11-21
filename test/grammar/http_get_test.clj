;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.http-get-test
  "Acceptance for [:http-get {:url u}]: the response body on stdout byte for
  byte, exit 0 on a 2xx answer, non-zero on a 4xx/5xx answer or a refused
  connection. The server is an in-JVM HttpServer on 127.0.0.1; runtimes are
  real shells whose PATH holds only the chosen fetch tool, so each
  realization is selected and run for real."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [grammar.core :as g]
            [grammar.fixtures :as fx]
            [grammar.profile :as p]
            [grammar.runtime :as rt]
            [grammar.schema :as s])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.io ByteArrayOutputStream File]
           [java.net InetAddress InetSocketAddress ServerSocket]
           [java.util Arrays]))

;; ---------------------------------------------------------------- server

(def body
  "NUL, a non-ASCII UTF-8 sequence, CR LF, every byte value, and more than
  64 KiB in all."
  (let [out (ByteArrayOutputStream.)
        put (fn [^bytes bs] (.write out bs 0 (alength bs)))]
    (put (.getBytes "head\u0000\u00e9\u03bb\ud83d\ude80\r\nline2\r\n" "UTF-8"))
    (put (byte-array (map unchecked-byte (range 256))))
    (dotimes [i 3000] (put (.getBytes (str "row " i " \u00fc\r\n") "UTF-8")))
    (.toByteArray out)))

(defn- respond [^HttpExchange ex code ^bytes bs]
  (.sendResponseHeaders ex code (if (zero? (alength bs)) -1 (alength bs)))
  (with-open [os (.getResponseBody ex)] (.write os bs)))

(def ^:private handler
  (reify HttpHandler
    (handle [_ ex]
      (let [path (.getRawPath (.getRequestURI ex))]
        (cond
          (= path "/body") (respond ex 200 body)
          (= path "/empty") (respond ex 200 (byte-array 0))
          (= path "/moved") (do (.add (.getResponseHeaders ex) "Location" "/body")
                                (respond ex 302 (byte-array 0)))
          (= path "/fail") (respond ex 500 (.getBytes "boom" "UTF-8"))
          (str/starts-with? path "/echo/") (respond ex 200 (.getBytes (str (.getRequestURI ex)) "UTF-8"))
          :else (respond ex 404 (.getBytes "not found" "UTF-8")))))))

(def ^:private server (atom nil))

(defn- base [] (str "http://127.0.0.1:" (.getPort (.getAddress ^HttpServer @server))))

(defn- with-server [f]
  (let [srv (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
    (.createContext srv "/" handler)
    (.start srv)
    (reset! server srv)
    (try (f) (finally (.stop srv 0) (reset! server nil)))))

(use-fixtures :once with-server)

(defn- dead-url
  "An http URL on a 127.0.0.1 port nothing listens on."
  []
  (let [port (with-open [ss (ServerSocket. 0 1 (InetAddress/getByName "127.0.0.1"))]
               (.getLocalPort ss))]
    (str "http://127.0.0.1:" port "/body")))

;; ---------------------------------------------------------------- runtimes

(def ^:private fetch-caps
  [:sh.cmd/curl :sh.cmd/wget :sh.cmd/busybox])

(defn- runtime!
  "Controlled sh runtime with tool-ks on PATH, discovered and with every
  fetch command resolved, so planning sees :yes or :no, never :unknown."
  [id shell tool-ks]
  (let [r (fx/sh-runtime! id shell tool-ks)]
    {:rt r :prof (g/resolve! r (g/discover! r) fetch-caps)}))

(def ^:private rows
  "[id shell tools expected-head]: one runtime per realization."
  [[:hg-curl     :dash    [:curl]    :sh.http-get/curl]
   [:hg-wget     :dash    [:wget]    :sh.http-get/wget]
   [:hg-busybox  :dash    [:busybox] :sh.http-get/busybox-wget]
   [:hg-bb-shell :busybox []         :applet]
   [:hg-python   :dash    [:python3] :ctx/sh->python]])

(def ^:private rts
  (delay (into {} (for [[id shell ks] rows] [id (runtime! id shell ks)]))))

(defn- expected-head
  "busybox sh resolves the wget applet with an empty PATH only when it is
  built to (busybox-static); a dynamically linked busybox does not. Follow
  the observed verdict."
  [head prof]
  (if (= :applet head)
    (when (= :yes (p/verdict prof :sh.cmd/wget)) :sh.http-get/wget)
    head))

(defn- fetch [id url]
  (let [{r :rt prof :prof} (@rts id)]
    (g/realize! r prof [:http-get {:url url}])))

(defn- planned-run
  "Plan, then run the planned code, keeping the raw stdout bytes."
  [id url]
  (let [{r :rt prof :prof} (@rts id)
        res (g/plan prof :sh [:http-get {:url url}])]
    (assoc res :run (when (= :ok (:status res)) (rt/execute r (:code res))))))

(defn- same-bytes? [^bytes a ^bytes b] (and (some? b) (Arrays/equals a b)))

;; ---------------------------------------------------------------- tests

(deftest schema-accepts-only-http-urls
  (doseq [u ["http://h/" "https://h/x?y=1" "http://127.0.0.1:1/a'b c$(id)"]]
    (is (s/intent? [:http-get {:url u}]) u))
  (doseq [u ["" "-o/tmp/x" "--next" "file:///etc/passwd" "ftp://h/" "HTTP://h/" "h/x"
             "http://h/a\u0000b" "http://h/a\r\nHost: x" "http://h/\u007f" " http://h/"]]
    (is (not (s/intent? [:http-get {:url u}])) (pr-str u)))
  (is (not (s/intent? [:http-get {:url "http://h/" :extra 1}])))
  (is (not (s/intent? [:http-get {}]))))

(deftest each-realization-fetches-byte-exact
  (doseq [[id _ _ head] rows]
    (testing (str id)
      (let [head (expected-head head (:prof (@rts id)))
            planned (planned-run id (str (base) "/body"))]
        (if-not head
          (is (= :unsatisfiable (:status planned)))
          (let [run (:run planned)]
            (is (= :ok (:status planned)) (pr-str (dissoc planned :explain)))
            (is (= head (first (:realization planned))))
            (is (= 0 (:exit run)) (:err run))
            (is (same-bytes? body (:out-bytes run))
                (str "got " (some-> ^bytes (:out-bytes run) alength) " bytes, want " (alength ^bytes body)))
            (testing "an empty 200 body is empty stdout, exit 0"
              (let [r (:run (planned-run id (str (base) "/empty")))]
                (is (= 0 (:exit r)) (:err r))
                (is (zero? (alength ^bytes (:out-bytes r))))))
            (testing "a redirect is followed"
              (let [r (:run (planned-run id (str (base) "/moved")))]
                (is (= 0 (:exit r)) (:err r))
                (is (same-bytes? body (:out-bytes r)))))
            (testing "realize! verifies on exit 0"
              (is (true? (:ok? (:verified (fetch id (str (base) "/body")))))))))))))

(deftest failures-exit-non-zero
  (doseq [[id _ _ head] rows
          :when (expected-head head (:prof (@rts id)))
          [what url] [["404" (str (base) "/missing")]
                      ["500" (str (base) "/fail")]
                      ["connection refused" (dead-url)]]]
    (testing (str id " " what)
      (let [res (fetch id url)]
        (is (= :ok (:status res)))
        (is (not= 0 (get-in res [:execution :exit])) (pr-str (:execution res)))
        (is (false? (:ok? (:verified res))))
        (is (= "" (get-in res [:execution :out])) "an error page is not a body")))))

(deftest urls-cannot-inject
  (let [canary (str (fx/tmp-dir! "grammar-hg-canary-") "/pwned")
        ;; Only characters a URI may hold, so every tool sends it and the
        ;; server parses it; $IFS stands in for the space a payload needs.
        legal (str "/echo/a'b$(touch$IFS" canary ");touch$IFS" canary ";-o" canary)
        ;; A raw space and backticks: tools may refuse it or the server may
        ;; answer 400, but nothing may run.
        raw (str "/echo/a'b c$(touch " canary ")`touch " canary "`|touch " canary " -o " canary)]
    (doseq [[id _ _ head] rows
            :when (expected-head head (:prof (@rts id)))]
      (testing (str id)
        (let [res (fetch id (str (base) legal))
              out (str (get-in res [:execution :out]))]
          (is (= :ok (:status res)))
          (is (= 0 (get-in res [:execution :exit])) (pr-str (:execution res)))
          (testing "the URL reached the server as one request for that path"
            (is (str/starts-with? out (str "/echo/a'b$(touch$IFS" canary ")")) out)))
        (let [res (fetch id (str (base) raw))]
          (is (= :ok (:status res)))
          (is (some? (get-in res [:execution :exit])) "it ran"))
        (is (not (.exists (File. canary))) "no shell expansion ran")))
    (testing "a leading - is no URL, so no tool can take it as an option"
      (doseq [u ["-o/tmp/x" "--output=/tmp/x" "-qO/tmp/x"]]
        (is (= :invalid (:status (g/plan (:prof (@rts :hg-curl)) :sh [:http-get {:url u}]))))))
    (testing "the URL is one quoted word after --"
      (is (= "'curl' '-q' '-fsSLg' '--' 'http://h/a'\\''b'"
             (:code (g/plan (:prof (@rts :hg-curl)) :sh [:http-get {:url "http://h/a'b"}])))))))

(deftest text-constraints-apply-to-the-url
  (let [prof (:prof (@rts :hg-curl))
        url "http://h/secret-path"]
    (testing "forbid sees the URL text"
      (is (= :unsatisfiable (:status (g/plan (p/declare-text prof :forbid #{"secret"} "test") :sh
                                            [:http-get {:url url}])))))
    (testing "max-len counts it"
      (let [ok (g/plan prof :sh [:http-get {:url url}])]
        (is (= :ok (:status ok)))
        (is (= :unsatisfiable (:status (g/plan (p/declare-text prof :max-len (dec (:bytes (:constraints ok))) "test")
                                              :sh [:http-get {:url url}]))))))
    (testing "a forbidden quote moves to another encoder, still one word"
      (let [res (g/plan (p/declare-text prof :forbid #{"'"} "test") :sh [:http-get {:url "http://h/a b"}])]
        (is (= :ok (:status res)))
        (is (= [:sh.http-get/curl :sh.lit/double] (vec (take 2 (:realization res)))))))))

(deftest python-direct-and-delegated
  (testing "a :python runtime fetches with urllib, byte for byte"
    (let [py (fx/python-runtime! :hg-py [])
          prof (g/discover! (fx/sh-runtime! :hg-py-probe :dash [:python3]))
          plan-for #(g/plan prof :python [:http-get {:url (str (base) %)}])
          res (plan-for "/body")
          run (rt/execute py (:code res))]
      (is (= [:py.http-get/urllib :py.lit/single] (:realization res)))
      (is (= 0 (:exit run)) (:err run))
      (is (same-bytes? body (:out-bytes run)))
      (is (not= 0 (:exit (rt/execute py (:code (plan-for "/missing"))))))))
  (testing "sh delegates to python through :ctx/sh->python, with no new wrapper"
    (let [res (g/plan (:prof (@rts :hg-python)) :sh [:http-get {:url (str (base) "/body")}])]
      (is (= [:ctx/sh->python :py.http-get/urllib] (vec (take 2 (:realization res)))))
      (is (= [:sh :python] (:stack res)))))
  (testing "nothing to fetch with: unsatisfiable, and nothing runs"
    (let [{r :rt prof :prof} (runtime! :hg-none :dash [])
          res (g/realize! r prof [:http-get {:url (str (base) "/body")}])]
      (is (= :unsatisfiable (:status res)))
      (is (nil? (:execution res))))))
