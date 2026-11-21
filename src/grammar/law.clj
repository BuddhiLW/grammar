;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.law
  "Laws a language module must keep, as plain checks a module author can
  call from a property test.

  The length law: no output of an encoder is shorter, in UTF-8 bytes, than
  the text it encodes, and no output of a realization or wrapper is shorter
  than any piece it embeds. grammar.solve relies on it to prune every
  candidate already over a profile's :max-len, together with everything
  that would be built from it. An entry that cannot keep it declares
  :shortens? true; pruning is then off beneath it, and the law does not
  apply to it.")

(defn utf8-len
  "Length of s in UTF-8 bytes."
  [^String s]
  (alength (.getBytes s "UTF-8")))

(defn- piece-env
  "A render env whose every literal is piece, so an output's length can be
  compared with the piece it embeds."
  [piece]
  {:lit (fn [_ _] [{:code piece :via [] :requires #{} :stack [] :cost 0 :key []}])})

(defn- shorter
  "The outputs of entry x's :render, called on arg with an env whose every
  literal is piece, that are shorter than floor bytes, as {:id :code}."
  [x piece floor arg]
  (for [cand ((:render x) (piece-env piece) arg)
        :when (< (utf8-len (:code cand)) floor)]
    {:id (:id x) :code (:code cand)}))

(defn length-law-violations
  "Every breach of the length law by the entries of registry
  ({:realizations [...] :wrappers [...]}, a composed module set) for one
  sample: text s for encoders, intent-of (fn [intent-kw s] intent) for
  other realizations, s as the inner code for wrappers, and piece as what
  every literal they ask for renders to. Entries declaring :shortens? are
  skipped. Empty when the law holds for this sample."
  [{:keys [realizations wrappers]} intent-of s piece]
  (let [keeps (remove :shortens? realizations)]
    (vec (concat
          (for [r keeps :when (= :lit (:intent r))
                v (shorter r piece (utf8-len s) [:lit s])]
            v)
          (for [r keeps :when (not= :lit (:intent r))
                v (shorter r piece (utf8-len piece) (intent-of (:intent r) s))]
            v)
          (for [w (remove :shortens? wrappers)
                v (shorter w piece (utf8-len piece) s)]
            v)))))
