;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 buddhilw

(ns grammar.lab
  "Real relay labs for the gated ssh tests: a temporary key pair, an
  ssh_config naming every lab host, and the nodes themselves.

  docker lab (GRAMMAR_SSHD=1): containers of test/lab/sshd on a private
  network, n1 published on 127.0.0.1 at a high port, n2 and n3 reachable
  only from inside the network (by ProxyJump or a nested ssh).

  Everything lives in one temp dir; stop! removes the containers and it.
  Nothing here prints a host key, an address or a key."
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]))

(defn- sh! [& args]
  (let [r (apply sh/sh args)]
    (when-not (zero? (:exit r))
      (throw (ex-info (str "lab command failed: " (first args) " " (second args))
                      {:exit (:exit r) :err (subs (:err r) 0 (min 400 (count (:err r))))})))
    (str/trim (:out r))))

(defn- tmp-dir! []
  (str (Files/createTempDirectory "grammar-lab-"
                                  (into-array FileAttribute [(PosixFilePermissions/asFileAttribute
                                                              (PosixFilePermissions/fromString "rwx------"))]))))

(defn keypair!
  "A fresh ed25519 key pair in dir: [private-path public-text]."
  [dir]
  (let [k (str dir "/id")]
    (sh! "ssh-keygen" "-q" "-t" "ed25519" "-N" "" "-C" "grammar-lab" "-f" k)
    [k (slurp (str k ".pub"))]))

(def image "grammar-relay-lab:1")

(defn image? [] (zero? (:exit (sh/sh "docker" "image" "inspect" image))))

(defn- free-port []
  (with-open [s (java.net.ServerSocket. 0 0 (java.net.InetAddress/getLoopbackAddress))] (.getLocalPort s)))

(defn- ssh-config
  "Host blocks for the lab. n1 through the published port, n2/n3 by name
  inside the network: a jump or a nested ssh reaches them."
  [{:keys [port key known dir]}]
  (str/join
   "\n"
   (concat
    ["Host *" (str "  IdentityFile " key) "  IdentitiesOnly yes" (str "  UserKnownHostsFile " known)
     "  ControlMaster auto" (str "  ControlPath " dir "/cm-%C") "  ControlPersist 120"
     "  StrictHostKeyChecking accept-new" "  LogLevel ERROR" "  ConnectTimeout 10" ""
     "Host n1" "  HostName 127.0.0.1" (str "  Port " port) "  User relay" ""])))

(defn start-docker!
  "Start the docker lab: nodes n1..n3, keys, config. Returns a lab map."
  []
  (let [dir (tmp-dir!)
        [k pub] (keypair! dir)
        tag (str "grammar-lab-" (System/currentTimeMillis))
        net (sh! "docker" "network" "create" tag)
        port (free-port)
        names (mapv #(str tag "-n" %) [1 2 3])]
    (doseq [[i n] (map-indexed vector names)]
      (apply sh! (concat ["docker" "run" "-d" "--rm" "--name" n "--network" tag "--network-alias" (str "n" (inc i))]
                          (when (zero? i) ["-p" (str "127.0.0.1:" port ":22")])
                          [image]))
      (spit (str dir "/pub") pub)
      (doseq [u ["relay" "cshu" "fishu" "app"]]
        (sh! "docker" "cp" (str dir "/pub") (str n ":/etc/ssh/keys/" u)))
      (sh! "docker" "exec" n "sh" "-c" "chown root:root /etc/ssh/keys/*; chmod 644 /etc/ssh/keys/*")
      ;; inner hops (n1 -> n2) need the same key inside the containers
      (sh! "docker" "cp" k (str n ":/etc/ssh/lab_id"))
      (sh! "docker" "exec" n "sh" "-c"
            (str "for u in relay cshu fishu app; do d=/home/$u/.ssh; mkdir -p $d; cp /etc/ssh/lab_id $d/id_ed25519;"
                 " printf '%s\\n' 'Host *' '  StrictHostKeyChecking accept-new' '  LogLevel ERROR' > $d/config;"
                 " chown -R $u $d; chmod 700 $d; chmod 600 $d/*; done")))
    (let [cfg (str dir "/config")]
      (spit cfg (ssh-config {:port port :key k :known (str dir "/known_hosts") :dir dir}))
      (.setReadable (File. cfg) true true)
      {:kind :docker :dir dir :config cfg :net net :containers names :tag tag})))

(defn wait-ready!
  "Poll until ssh to n1 answers, at most tries times."
  [{:keys [config]} tries]
  (loop [i 0]
    (let [r (sh/sh "ssh" "-F" config "-T" "-o" "BatchMode=yes" "--" "n1" "true")]
      (cond (zero? (:exit r)) true
            (>= i tries) false
            :else (do (Thread/sleep 500) (recur (inc i)))))))

(defn stop!
  "Remove the lab's containers, network and temp dir."
  [{:keys [kind dir containers net config]}]
  (when config (sh/sh "ssh" "-F" config "-O" "exit" "n1"))
  (when (= kind :docker)
    (doseq [c containers] (sh/sh "docker" "rm" "-f" c))
    (when net (sh/sh "docker" "network" "rm" net)))
  (when dir (sh/sh "rm" "-rf" "--" dir))
  nil)
