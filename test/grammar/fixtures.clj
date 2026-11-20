;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.fixtures
  "Controlled runtimes: real interpreters whose PATH is cut down to a
  scratch bin dir holding only the chosen tools. Restrictions are produced
  for real (bash -r really refuses redirection, a missing python3 is really
  missing), never simulated."
  (:require [grammar.runtime :as rt])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn which
  "Canonical path of tool on the host PATH, or nil."
  [tool]
  (some (fn [dir]
          (let [f (File. ^String dir ^String tool)]
            (when (and (.isFile f) (.canExecute f)) (.getCanonicalPath f))))
        (.split ^String (System/getenv "PATH") File/pathSeparator)))

(def tools
  "Tool name -> host binary the scratch bin dir links to."
  {:sh "/usr/bin/dash"
   :tee (which "tee")
   :python3 (which "python3")
   :mkdir (which "mkdir")
   :rm (which "rm")})

(defn tmp-dir! [prefix]
  (str (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn bin-dir!
  "Fresh directory with a symlink for each tool in ks, nothing else."
  [ks]
  (let [dir (File. ^String (tmp-dir! "grammar-bin-"))]
    (doseq [k ks]
      (Files/createSymbolicLink (.toPath (File. dir (name k)))
                                (.toPath (File. ^String (tools k)))
                                (make-array FileAttribute 0)))
    (str dir)))

(def shells
  {:bash ["/usr/bin/bash" "-c"]
   :rbash ["/usr/bin/bash" "-r" "-c"]
   :dash ["/usr/bin/dash" "-c"]
   :busybox ["/usr/bin/busybox" "sh" "-c"]})

(defn sh-runtime!
  "An :sh runtime: shell from `shells`, PATH = bin dir of tool keys."
  [id shell tool-ks]
  (rt/process-runtime {:id id :lang :sh :argv (shells shell)
                       :env {"PATH" (bin-dir! tool-ks) "LC_ALL" "C.UTF-8"}}))

(defn python-runtime!
  "A :python runtime (python3 -c), PATH = bin dir of tool keys."
  [id tool-ks]
  (rt/process-runtime {:id id :lang :python :argv [(tools :python3) "-c"]
                       :env {"PATH" (bin-dir! tool-ks) "LC_ALL" "C.UTF-8"}}))

(def catalog
  "Controlled sh runtimes: [id shell tools]."
  [[:bash-full    :bash    [:sh :tee :python3]]
   [:dash-bare    :dash    []]
   [:rbash-tee    :rbash   [:tee]]
   [:rbash-python :rbash   [:python3]]
   [:rbash-none   :rbash   []]
   [:busybox-bare :busybox []]])

(defn runtimes!
  "Fresh runtimes for the whole catalog, keyed by id."
  []
  (into {} (for [[id shell ks] catalog] [id (sh-runtime! id shell ks)])))
