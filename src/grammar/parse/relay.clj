;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.parse.relay
  "Pure oracle for relay chains: what a program text does when it is run
  through a sequence of hops, each re-parsing it its own way.

  A hop is one of
    sh -c X            a shell parses X (dialect :posix; bash -c: :bash)
    env -i A=B.. CMD   env execs CMD: no parse
    sudo -n [-u U] -- CMD   sudo execs CMD as U: no parse
    ssh [opts] -- HOST W..  ssh JOINS W.. with spaces and HOST's login
                       shell parses the result (its dialect, from the
                       world). -J J1,J2 forwards TCP through the jump
                       hosts: they are reached, never parse
    python3 -c P       python evaluates P (grammar.parse.py shapes)
    printf FMT [ARG]   the leaf: output bytes

  run returns {:out bytes :trace [...]} or {:error msg :trace [...]}. The
  trace has one row per parse, {:host :user :parser :input}, so a test can
  count how many times a payload was re-parsed and by which dialect.

  A world says what each host's login shell is:
    {:hosts {\"vm1\" {:login :posix} \"csh-box\" {:login :csh}}}
  The start context is {:host \"local\" :user \"me\"}."
  (:require [clojure.string :as str]
            [grammar.parse.py :as py]
            [grammar.parse.sh :as psh]))

(defn- refuse [msg data]
  (throw (ex-info msg (assoc data ::refuse true))))

(defn- utf8 [^String s] (vec (.getBytes s "UTF-8")))

(defn- printf-format
  "Bytes printf writes for a FORMAT with no arguments: \\ooo, \\\\ and %%
  are the only escapes modeled."
  [^String f]
  (when (str/starts-with? f "-") (refuse "printf: format looks like an option" {:format f}))
  (loop [i 0 out []]
    (if (>= i (count f))
      out
      (let [c (.charAt f i)]
        (case c
          \\ (let [ds (take-while #(and % (<= (int \0) (int %) (int \7)))
                                  (map #(when (< % (count f)) (.charAt f %)) (range (inc i) (+ i 4))))]
               (cond (seq ds) (recur (+ i 1 (count ds)) (conj out (unchecked-byte (Integer/parseInt (apply str ds) 8))))
                     (and (< (inc i) (count f)) (= \\ (.charAt f (inc i)))) (recur (+ i 2) (conj out 92))
                     :else (refuse "printf: escape not modeled" {:format f :at i})))
          \% (if (and (< (inc i) (count f)) (= \% (.charAt f (inc i))))
               (recur (+ i 2) (conj out 37))
               (refuse "printf: directive with no argument" {:format f :at i}))
          (recur (inc i) (into out (utf8 (str c)))))))))

(declare exec-argv eval-sh)

(defn- trace! [st row] (swap! (:trace st) conj row))

(defn- split-assignments [words]
  (split-with #(re-matches #"[A-Za-z_][A-Za-z0-9_]*=.*" %) words))

(def ^:private ssh-arg-opts #{"-o" "-J" "-p" "-i" "-F" "-l"})
(def ^:private ssh-flag-opts #{"-T" "-n" "-q" "-4" "-6" "-x" "-a"})

(defn- parse-ssh
  "{:host :user :jumps :words} from ssh argv after the program word."
  [args]
  (loop [[a & more :as as] args opts {}]
    (cond
      (nil? a) (refuse "ssh: no host" {:argv args})
      (= a "--") (let [[h & ws] more] (when-not h (refuse "ssh: no host" {:argv args}))
                   (assoc opts :dest h :words (vec ws)))
      (ssh-arg-opts a) (do (when-not (seq more) (refuse "ssh: option needs an argument" {:opt a}))
                           (recur (rest more) (update opts (keyword (subs a 1)) (fnil conj []) (first more))))
      (ssh-flag-opts a) (recur more opts)
      (str/starts-with? a "-") (refuse "ssh: option not modeled" {:opt a})
      :else (assoc opts :dest a :words (vec more)))))

(defn- host-of [world dest]
  (let [[u h] (if (str/includes? dest "@") (str/split dest #"@" 2) [nil dest])]
    (when-not (get-in world [:hosts h]) (refuse "ssh: unknown host" {:host h}))
    [u h]))

(defn- exec-ssh [st ctx args]
  (let [{:keys [dest words J l]} (parse-ssh args)
        world (:world st)
        _ (doseq [j (mapcat #(str/split % #",") J)] (host-of world j))
        [u h] (host-of world dest)
        login (get-in world [:hosts h :login] :posix)]
    (when (empty? words) (refuse "ssh: no remote command (would open a login session)" {:host h}))
    (eval-sh st (cond-> {:host h :user (or u (first l) (:user ctx))}
                  (:stdin ctx) (assoc :stdin (:stdin ctx)))
             login (str/join " " words))))

(defn- eval-py [st ctx code]
  (trace! st {:host (:host ctx) :user (:user ctx) :parser :python :input code})
  (let [{:keys [op error] :as p} (py/parse-program code)]
    (case op
      :write (utf8 (:text p))
      :exec (eval-py st ctx (:code p))
      :call (exec-argv st ctx (:argv p))
      (refuse (str "python: " error) {:code code}))))

(defn exec-argv
  "Bytes written by executing argv in ctx."
  [st ctx [prog & args :as argv]]
  (case prog
    "printf" (case (count args)
               1 (printf-format (first args))
               2 (if (= "%s" (first args)) (utf8 (second args))
                     (refuse "printf: format not modeled" {:argv argv}))
               (refuse "printf: arity not modeled" {:argv argv}))
    ("sh" "dash") (cond (= "-c" (first args)) (eval-sh st (dissoc ctx :stdin) :posix (second args))
                        (and (empty? args) (:stdin ctx))
                        (eval-sh st (dissoc ctx :stdin) :posix (String. (byte-array (:stdin ctx)) "UTF-8"))
                        :else (refuse "sh: only sh -c, or sh reading a script on stdin, is modeled" {:argv argv}))
    "bash" (if (= "-c" (first args)) (eval-sh st ctx :bash (second args))
               (refuse "bash: only bash -c is modeled" {:argv argv}))
    "env" (let [[i & more] args]
            (when-not (= "-i" i) (refuse "env: only env -i is modeled" {:argv argv}))
            (let [[_ cmd] (split-assignments more)]
              (when (empty? cmd) (refuse "env: no command" {:argv argv}))
              (exec-argv st ctx (vec cmd))))
    "sudo" (let [[n & more] args
                 _ (when-not (= "-n" n) (refuse "sudo: -n required (no password prompt)" {:argv argv}))
                 [user more] (if (= "-u" (first more)) [(second more) (drop 2 more)] ["root" more])
                 _ (when-not (= "--" (first more)) (refuse "sudo: -- required" {:argv argv}))
                 cmd (vec (rest more))]
             (when (empty? cmd) (refuse "sudo: no command" {:argv argv}))
             (exec-argv st (assoc ctx :user user) cmd))
    "ssh" (exec-ssh st ctx args)
    "python3" (if (= "-c" (first args)) (eval-py st ctx (second args))
                  (refuse "python3: only python3 -c is modeled" {:argv argv}))
    (refuse "program not modeled" {:argv argv})))

(defn eval-sh
  "Bytes written by shell text code parsed in dialect in ctx. A two-stage
  pipeline feeds the first command's output to the second's stdin."
  [st ctx dialect code]
  (when (nil? code) (refuse "sh -c: no program" {}))
  (trace! st {:host (:host ctx) :user (:user ctx) :parser dialect :input code})
  (let [{:keys [pipeline error at]} (psh/parse dialect code)]
    (cond
      error (refuse (str (name dialect) ": " error) {:code code :at at})
      (= 1 (count pipeline)) (exec-argv st ctx (first pipeline))
      (= 2 (count pipeline)) (let [in (exec-argv st (dissoc ctx :stdin) (first pipeline))]
                               (exec-argv st (assoc ctx :stdin in) (second pipeline)))
      :else (refuse "pipelines longer than two are not modeled" {:code code}))))

(defn run
  "Run program text code in language lang (:sh or :python; default :sh,
  parsed :posix) from the start context of world. {:out bytes :trace}
  or {:error msg :data map :trace}."
  ([code] (run {:hosts {"local" {:login :posix}}} :sh code))
  ([world lang code]
   (let [st {:world world :trace (atom [])}
         ctx {:host "local" :user "me"}]
     (try
       {:out (case lang
               :sh (eval-sh st ctx (get-in world [:hosts "local" :sh] :posix) code)
               :python (eval-py st ctx code))
        :trace @(:trace st)}
       (catch clojure.lang.ExceptionInfo e
         (if (::refuse (ex-data e))
           {:error (ex-message e) :data (dissoc (ex-data e) ::refuse) :trace @(:trace st)}
           (throw e)))))))

(defn out-string [{:keys [out]}]
  (when out (String. (byte-array out) "UTF-8")))
