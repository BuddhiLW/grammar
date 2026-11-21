;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.schema
  "Value objects. Every other namespace speaks in these shapes.

  Intent      what the caller wants, language-neutral.
  Verdict     tri-state truth of a capability: :yes, :no, :unknown.
  Evidence    why a verdict (or a text constraint) holds: a probe run with
              its exit status and output, or an explicit declaration.
  Profile     capabilities + text constraints + the evidence behind them.
  Result      what the planner returns, always carrying its justification."
  (:require [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me]))

(def Lang
  "A target language: :sh, :python, or any language a module registers
  (see grammar.core/modules). Open, so a new language is data, not a schema
  edit."
  :keyword)

(def Str [:tuple [:= :str] :string])

(def WriteFile
  [:tuple [:= :write-file]
   [:map {:closed true} [:path :string] [:content :string]]])

(def ArgvWord
  "An exec argument: any string without NUL, which no argv entry can hold."
  [:and :string [:fn {:error/message "argv words cannot contain NUL"}
                 #(not (.contains ^String % "\u0000"))]])

(def Run
  [:tuple [:= :run]
   [:map {:closed true} [:argv [:vector {:min 1} ArgvWord]]]])

(def PathArg
  "A filesystem path an operation acts on: non-empty, no NUL."
  [:and ArgvWord [:string {:min 1}]])

(def EnsureDir
  [:tuple [:= :ensure-dir] [:map {:closed true} [:path PathArg]]])

(def RemoveFile
  [:tuple [:= :remove-file] [:map {:closed true} [:path PathArg]]])

(defn http-url?
  "True for a string naming an http:// or https:// URL (scheme in lower case)
  with no ASCII control character. Every other scheme (file:, ftp:, ...) and
  every option-like word (a leading -) is refused, so no fetch tool can read
  it as anything but an http URL; a control character (CR LF above all) could
  split the HTTP request a fetch tool writes, so it is refused too."
  [s]
  (and (string? s)
       (or (str/starts-with? s "http://") (str/starts-with? s "https://"))
       (not-any? #(let [c (int %)] (or (< c 32) (= c 127))) s)))

(def HttpUrl
  "A URL an :http-get fetches: http:// or https://, no control characters."
  [:fn {:error/message "only http:// and https:// URLs without control characters are accepted"}
   http-url?])

(def HttpGet
  [:tuple [:= :http-get] [:map {:closed true} [:url HttpUrl]]])

(def Intent
  [:multi {:dispatch first}
   [:str Str]
   [:write-file WriteFile]
   [:run Run]
   [:ensure-dir EnsureDir]
   [:remove-file RemoveFile]
   [:http-get HttpGet]])

(def Verdict [:enum :yes :no :unknown])

(def Evidence
  [:map
   [:id :keyword]
   [:kind [:enum :probe :declared]]
   [:cap {:optional true} :keyword]
   [:constraint {:optional true} :keyword]
   [:verdict {:optional true} Verdict]
   [:runtime {:optional true} :keyword]
   [:argv {:optional true} [:vector :string]]
   [:script {:optional true} :string]
   [:expect {:optional true} :string]
   [:exit {:optional true} [:maybe :int]]
   [:out {:optional true} :string]
   [:err {:optional true} :string]
   [:error {:optional true} :string]
   [:reason {:optional true} :string]])

(def CapEntry
  [:map [:verdict Verdict] [:evidence [:vector :keyword]]])

(def TextConstraints
  [:map
   [:max-len {:optional true} pos-int?]
   [:forbid {:optional true} [:set :string]]
   [:evidence {:optional true} [:vector :keyword]]])

(def Profile
  [:map
   [:id :keyword]
   [:caps [:map-of :keyword CapEntry]]
   [:text TextConstraints]
   [:evidence [:map-of :keyword Evidence]]])

(def Result
  [:map
   [:status [:enum :ok :unsatisfiable :blocked-on-unknown :invalid]]
   [:intent :any]
   [:lang {:optional true} Lang]
   [:profile {:optional true} :keyword]
   [:code {:optional true} :string]
   [:realization {:optional true} [:vector :keyword]]
   [:stack {:optional true} [:vector Lang]]
   [:constraints {:optional true} :map]
   [:evidence [:vector :keyword]]
   [:unknowns [:vector :keyword]]
   [:would-select {:optional true} [:vector :keyword]]
   [:needs {:optional true} [:vector :keyword]]
   [:rejected {:optional true} [:vector :map]]
   [:explain {:optional true} [:vector :map]]
   [:searched {:optional true} :int]
   [:pruned {:optional true} :int]
   [:errors {:optional true} :any]])

(defn intent? [x] (m/validate Intent x))
(defn explain-intent [x] (me/humanize (m/explain Intent x)))
(defn profile? [x] (m/validate Profile x))
(defn result? [x] (m/validate Result x))
(defn explain-result [x] (me/humanize (m/explain Result x)))
