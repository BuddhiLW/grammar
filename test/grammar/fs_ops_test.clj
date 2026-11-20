;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.fs-ops-test
  "Acceptance for :ensure-dir and :remove-file: one intent, the program the
  runtime can actually run. coreutils when they are on PATH, python pathlib
  through a context wrapper when they are not, nothing when neither is."
  (:require [clojure.test :refer [deftest is testing]]
            [grammar.core :as g]
            [grammar.fixtures :as fx])
  (:import [java.io File]))

(def ^:private runtimes
  [[:coreutils :dash  [:mkdir :rm]       :sh.ensure-dir/mkdir   :sh.remove-file/rm]
   [:rbash-py  :rbash [:python3]         :ctx/sh->python        :ctx/sh->python]
   [:busybox   :busybox []               :sh.ensure-dir/mkdir   :sh.remove-file/rm]
   [:bare      :dash  []                 nil                    nil]])

(deftest same-op-different-programs
  (let [out (fx/tmp-dir! "grammar-fs-")]
    (doseq [[id shell tools mk-head rm-head] runtimes]
      (testing (str id)
        (let [rt (fx/sh-runtime! id shell tools)
              prof (g/discover! rt)
              dir (str out "/" (name id) "/it's $HOME/a b")
              file (str out "/" (name id) ".txt")
              _ (spit file "x")
              mk (g/realize! rt prof [:ensure-dir {:path dir}] {:resolve true})
              rm (g/realize! rt (:profile-after mk) [:remove-file {:path file}] {:resolve true})]
          (if mk-head
            (do (is (= mk-head (first (:realization mk))))
                (is (true? (:ok? (:verified mk))) (pr-str (:execution mk)))
                (is (.isDirectory (File. dir)))
                (is (= rm-head (first (:realization rm))))
                (is (true? (:ok? (:verified rm))) (pr-str (:execution rm)))
                (is (not (.exists (File. file)))))
            (do (is (= :unsatisfiable (:status mk)))
                (is (= :unsatisfiable (:status rm)))
                (is (.exists (File. file)) "nothing ran"))))))))

(deftest idempotent
  (let [out (fx/tmp-dir! "grammar-fs-idem-")
        rt (fx/sh-runtime! :idem :dash [:mkdir :rm])
        prof (g/resolve! rt (g/discover! rt) [:sh.cmd/mkdir :sh.cmd/rm])]
    (testing "ensuring an existing directory succeeds"
      (dotimes [_ 2]
        (is (true? (:ok? (:verified (g/realize! rt prof [:ensure-dir {:path (str out "/d")}])))))))
    (testing "removing an absent file succeeds"
      (is (true? (:ok? (:verified (g/realize! rt prof [:remove-file {:path (str out "/none")}]))))))
    (testing "a leading dash is a path, not an option"
      (let [res (g/plan prof :sh [:ensure-dir {:path "-p"}])]
        (is (= "'mkdir' '-p' '--' '-p'" (:code res)))))))
