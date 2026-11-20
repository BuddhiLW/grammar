(ns grammar.run-test
  "Acceptance for [:run {:argv [...]}]: every argv element reaches the
  program as exactly one word, through any context stack, and the program
  named by argv[0] is resolved on demand, never assumed."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [grammar.core :as g]
            [grammar.fixtures :as fx]
            [grammar.probe :as probe]
            [grammar.profile :as p]
            [grammar.roundtrip-test :as rtt]
            [grammar.schema :as s]
            [grammar.runtime]))

(def echo-argv
  "python3 -c program printing its arguments NUL-separated."
  "import sys;sys.stdout.write('\\x00'.join(sys.argv[1:]))")

(defn- echo-intent [args]
  [:run {:argv (into ["python3" "-c" echo-argv] args)}])

(def ^:private dash
  (delay (let [rt (fx/sh-runtime! :run-dash :dash [:sh :python3])]
           {:rt rt :prof (g/resolve! rt (g/discover! rt) [:bin/python3])})))

(defspec argv-words-roundtrip-through-stacks 60
  (prop/for-all [args (gen/vector rtt/gen-text 0 4)
                 stack rtt/gen-stack]
    (let [{:keys [rt prof]} @dash
          res (g/realize! rt prof (echo-intent args) {:stack stack})]
      (and (= :ok (:status res))
           (= stack (:stack res))
           (true? (:ok? (:verified res)))
           (= (str/join "\u0000" args) (get-in res [:execution :out]))))))

(deftest program-resolved-on-demand
  (doseq [[id rt] (fx/runtimes!)]
    (let [prof (g/discover! rt)
          has-python? (some #{:python3} (some (fn [[cid _ ks]] (when (= cid id) ks)) fx/catalog))
          intent (echo-intent ["a b" "'"])]
      (testing (str id)
        (testing "plain planning never assumes the program exists"
          (let [res (g/plan prof :sh intent)]
            (is (= :blocked-on-unknown (:status res)))
            (is (= [:bin/python3] (:needs res)))))
        (testing "resolving probes it once, with traceable evidence"
          (let [res (g/realize! rt prof intent {:resolve true})
                after (:profile-after res)
                ev (get-in after [:evidence (probe/evidence-id id :bin/python3)])]
            (is (s/result? (dissoc res :runtime :profile-after :execution :verified)))
            (is (= (if has-python? :yes :no) (p/verdict after :bin/python3)))
            (is (= :probe (:kind ev)))
            (is (int? (:exit ev)))
            (if has-python?
              (do (is (= :ok (:status res)))
                  (is (true? (:ok? (:verified res))))
                  (is (= "a b\u0000'" (get-in res [:execution :out]))))
              (do (is (= :unsatisfiable (:status res)))
                  (is (nil? (:execution res)) "nothing runs when the plan fails")))))))))

(deftest refusals
  (let [{:keys [prof]} @dash]
    (testing "an argv word cannot carry NUL, in any language"
      (doseq [stack [[:sh] [:sh :python]]]
        (is (= :invalid (:status (g/plan prof :sh (echo-intent ["a\u0000b"]) {:stack stack}))))))
    (testing "empty argv is not an intent"
      (is (= :invalid (:status (g/plan prof :sh [:run {:argv []}])))))
    (testing "a builtin is not a program on PATH"
      (let [rt (fx/sh-runtime! :run-bare :dash [])
            after (g/resolve! rt (g/discover! rt) [:bin/printf])]
        (is (= :no (p/verdict after :bin/printf)))))
    (testing "a builtin does not hide the executable of the same name on PATH"
      (let [rt (grammar.runtime/process-runtime {:id :run-usr :argv ["/usr/bin/dash" "-c"]
                                                 :env {"PATH" "/usr/bin:/bin" "LC_ALL" "C.UTF-8"}})
            after (g/resolve! rt (g/discover! rt) [:bin/printf :bin/tee])]
        (is (= :yes (p/verdict after :bin/printf)))))
    (testing "an absolute program is checked as a file"
      (let [rt (fx/sh-runtime! :run-abs :dash [])
            after (g/resolve! rt (g/discover! rt) [(keyword "bin" "/usr/bin/dash") (keyword "bin" "/nope/x")])]
        (is (= :yes (p/verdict after (keyword "bin" "/usr/bin/dash"))))
        (is (= :no (p/verdict after (keyword "bin" "/nope/x"))))))))
