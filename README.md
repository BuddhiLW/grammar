# grammar

[![Clojars Project](https://img.shields.io/clojars/v/io.github.buddhilw/grammar.svg)](https://clojars.org/io.github.buddhilw/grammar)
[![release](https://github.com/BuddhiLW/grammar/actions/workflows/release.yml/badge.svg)](https://github.com/BuddhiLW/grammar/actions/workflows/release.yml)
[![ci](https://github.com/BuddhiLW/grammar/actions/workflows/ci.yml/badge.svg)](https://github.com/BuddhiLW/grammar/actions/workflows/ci.yml)

Plan a language-neutral intent into a program that is valid for one observed
runtime: the shell it runs in, the tools on its PATH, the syntax it accepts,
the characters and length a channel allows.

v1 proves one claim: **one intent produces different valid implementations
under different observed restrictions, with no handwritten per-environment
commands.**

## Install

`deps.edn`, with the version from the Clojars badge above:

```edn
io.github.buddhilw/grammar {:mvn/version "0.1.1"}
```

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
- `[:run {:argv ["prog" "arg" ...]}]`: execute `prog` with each argv element as
  exactly one word (no NUL, which no argv can carry). The program is a
  capability, `:bin/prog`, resolved on demand: plain planning returns
  `:blocked-on-unknown` with `:needs [:bin/prog]`; `realize!` with
  `{:resolve true}` (or `plan-resolving!`) probes it in the runtime, records
  the evidence and plans again. Verified by exit status 0.

## On-demand capabilities

The fixed probe catalog covers the shell's grammar. Capabilities that depend
on the intent (`:bin/<program>`) are probed when a plan needs them:
`grammar.probe/on-demand` gives the probe for a language, `grammar.core/resolve!`
runs it. A realization adds intent-dependent requirements through
`:requires-of (fn [intent] caps)`.

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

## Release

Built and published with [hive-build](https://clojars.org/io.github.hive-agi/hive-build)
(`clojure -T:build <task>`; coordinates in `version.edn`, version floor in
`VERSION`).

- `ci.yml`: the suite, the licence check and a jar build on every pull request
  and every push outside `main`.
- `release.yml`: on a push to `main` that changes the library (src, test,
  deps, version, licence, workflows), runs the suite, checks the licence, tags
  `v<patch+1>` of the newest tag and deploys to Clojars. Nothing is committed
  back to `main`. Secrets: `CLOJARS_USERNAME`, `CLOJARS_DEPLOY_TOKEN`.

## License

Copyright (C) 2026 buddhilw

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program (see [LICENSE](LICENSE)). If not, see
<https://www.gnu.org/licenses/>.

Every source file carries `SPDX-License-Identifier: GPL-3.0-or-later`.
