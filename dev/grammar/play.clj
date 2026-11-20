(ns grammar.play
  "REPL playground. C-c C-e / C-x C-e each form in the comment block."
  (:require [grammar.core :as g]
            [grammar.fixtures :as fx]
            [grammar.profile :as p]
            [grammar.runtime :as rt]))

(comment
  ;; 1. Build the controlled runtimes and discover each one for real.
  (def rts (fx/runtimes!))
  (def profs (update-vals rts g/discover!))

  ;; Verdict table: who can do what, as observed.
  (into (sorted-map) (update-vals profs #(into (sorted-map) (update-vals (:caps %) :verdict))))

  ;; Why does rbash refuse redirection? The evidence, with exit status and stderr.
  (get-in profs [:rbash-none :evidence :ev.rbash-none/sh.redirect])

  ;; 2. One intent, every runtime.
  (def intent [:write-file {:path "/tmp/grammar-play.txt" :content "it's \"x\" $HOME\nλ 🚀\n"}])
  (into (sorted-map)
        (for [[id r] rts]
          [id (select-keys (g/realize! r (profs id) intent) [:status :realization :code :verified])]))

  ;; 3. Pure planning: no process runs.
  (g/plan (profs :rbash-python) :sh intent)

  ;; 4. Declared policy on top of discovery.
  (-> (profs :dash-bare)
      (p/declare-text :forbid #{"'"} "no single quotes in this channel")
      (g/plan :sh intent)
      (select-keys [:status :code :realization :evidence]))

  (-> (profs :bash-full)
      (p/declare-text :max-len 40 "tiny command field")
      (g/plan :sh intent)
      (select-keys [:status :rejected]))

  ;; 5. Unknown stays unknown: a profile that never probed redirection.
  (-> (p/empty-profile :fresh)
      (p/declare-cap :sh/printf :yes "assumed")
      (p/declare-cap :sh/single-quote :yes "assumed")
      (g/plan :sh intent)
      (select-keys [:status :needs :would-select]))

  ;; 6. Context stacks: sh -> python -> sh -> printf, then run it.
  (let [res (g/realize! (rts :bash-full) (profs :bash-full) [:str "a'b\"c\n"] {:stack [:sh :python :sh]})]
    (select-keys res [:stack :realization :code :verified])))
