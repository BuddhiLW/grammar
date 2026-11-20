# grammar

Plan a language-neutral intent into a program that is valid for one observed
runtime: the shell it runs in, the tools on its PATH, the syntax it accepts,
the characters and length a channel allows.

v1 proves one claim: **one intent produces different valid implementations
under different observed restrictions, with no handwritten per-environment
commands.**

## Pieces

| Namespace          | Stratum   | Role |
|--------------------|-----------|------|
| `grammar.schema`   | values    | malli shapes: Intent, Verdict, Evidence, Profile, Result |
| `grammar.lit`      | pure      | literal encoders (sh single/double/ANSI-C, printf FORMAT, python) |
| `grammar.profile`  | pure      | tri-state capability profile; evidence-backed verdicts |
| `grammar.realize`  | data      | realizations per intent × language, context wrappers |
| `grammar.solve`    | pure      | deterministic search, text constraints, explanations |
| `grammar.probe`    | pure      | discovery laws as scripts; verdicts from observed runs |
| `grammar.runtime`  | boundary  | `IRuntime` port + `ProcessRuntime` adapter |
| `grammar.core`     | facade    | `discover!`, `plan`, `realize!`, `verify` |

## Intents

- `[:str s]`: emit exactly the bytes of `s` on stdout. Proves literal correctness.
- `[:write-file {:path p :content c}]`: `p` holds exactly `c`. Proves planning,
  rendering, execution and verification together.

## Capabilities are tri-state

`:yes` / `:no` / `:unknown`. A capability nobody probed is `:unknown` and stays
`:unknown`; it never selects a program. When only unknowns block every option,
the result is `:blocked-on-unknown` with `:needs` (what to probe) and
`:would-select` (what would run). Every verdict keeps the ids of the evidence
behind it: the probe script, exit status, stdout, stderr, or a declared reason.

## Every result carries

`:status`, `:realization` (ids, pre-order), `:stack`, `:constraints`
(`:requires`, text constraints, byte length), `:evidence` (ids into the
profile), `:unknowns`, and on failure `:explain` (every option, its required
caps split by verdict) and `:rejected` (text-constraint violations).

## Use

```clojure
(require '[grammar.core :as g] '[grammar.runtime :as rt])
(def r (rt/process-runtime {:id :box :argv ["/usr/bin/bash" "-r" "-c"]
                            :env {"PATH" "/opt/allowed/bin" "LC_ALL" "C.UTF-8"}}))
(def prof (g/discover! r))
(g/realize! r prof [:write-file {:path "/tmp/x" :content "it's $HOME\n"}])
```

Interactive tour: `dev/grammar/play.clj`.

## Test

```sh
clojure -M:test
```

Runs against real `bash`, `bash -r`, `dash`, `busybox sh` and `python3`, with
PATH cut down per runtime to a scratch bin dir.
