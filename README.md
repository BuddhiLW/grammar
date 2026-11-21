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
| `grammar.core`     | facade    | `discover!`, `plan`, `realize!`, `verify`, `effect` |

## Intents

- `[:str s]`: emit exactly the bytes of `s` on stdout. Proves literal correctness.
- `[:write-file {:path p :content c}]`: `p` holds exactly `c`. Proves planning,
  rendering, execution and verification together.
- `[:run {:argv ["prog" "arg" ...]}]`: execute `prog` with each argv element as
  exactly one word (no NUL, which no argv can carry). The program is a
  capability resolved on demand: plain planning returns `:blocked-on-unknown`
  with `:needs [:sh.cmd/prog]`; `realize!` with `{:resolve true}` (or
  `plan-resolving!`) probes it in the runtime, records the evidence and plans
  again. Verified by exit status 0.
- `[:ensure-dir {:path p}]`: `p` is a directory (parents created, idempotent).
  `mkdir -p --` when the shell has `mkdir`, else python `pathlib` through a
  context wrapper.
- `[:remove-file {:path p}]`: `p` is not a file (absent is fine). `rm -f --`
  or python `pathlib.unlink(missing_ok=True)`.
- `[:http-get {:url u}]`: write the response body to stdout byte for byte;
  exit 0 on a 2xx answer, non-zero on a 4xx/5xx answer or a connection
  failure, a body shorter than its `Content-Length` included. 301, 302, 303
  and 307 redirects are followed (308 too, except by python urllib before
  3.11). Only `http://` and `https://` URLs without control characters are
  accepted, so a URL can never start with `-`. In order of cost:
  `curl -q -fsSLg --` (`:sh.cmd/curl`), `wget --no-config --tries=1 -qO- --`
  (`:sh.cmd/wget` plus `:sh.wget/gnu`, probed from `wget --version`),
  `wget -qO- --` (a wget not proven GNU, e.g. the busybox applet),
  `busybox wget -qO- --` (`:sh.cmd/busybox`), python `urllib.request`
  (`:py/urllib`), directly or through `:ctx/sh->python`. busybox wget does
  not verify TLS certificates, so the last two wget forms decline `https://`
  URLs; on a runtime whose only fetcher is busybox, an https get is
  unsatisfiable. The URL is a literal like any other string, so
  `:forbid` and `:max-len` apply to it. Verified by exit status 0; the tools
  differ on URLs a URI cannot hold as written (a raw space: GNU wget
  percent-encodes it, curl and urllib refuse it, busybox sends it and the
  server answers 400).

`(grammar.core/effect intent)` observes, with no run, whether a state intent
(`:write-file`, `:ensure-dir`, `:remove-file`) already holds:
`{:check kw :holds? bool}`, or `nil` for `:str`, `:run` and `:http-get`. A caller can skip
work that is already done. `verify` is that same observation after a run,
plus exit status 0. Both read the local filesystem, so they only apply to a
runtime that shares it.

## On-demand capabilities

The fixed probe catalog covers the shell's grammar. Capabilities that depend
on the intent are probed when a plan needs them:

| Capability       | Holds when                                              | Needed by |
|------------------|---------------------------------------------------------|-----------|
| `:sh.cmd/<prog>` | the shell resolves the word (`command -v`): PATH, builtins, busybox applets | sh simple commands |
| `:bin/<prog>`    | exec finds an executable file on PATH (`[ -f ] && [ -x ]`, `shutil.which`) | python `subprocess` |

`grammar.probe/on-demand` gives the probe for a language, `grammar.core/resolve!`
runs it. A realization adds intent-dependent requirements through
`:requires-of (fn [intent] caps)`.

## Discovery by runtime language

A probe carries `:lang`, the language its script is written in (`:sh` when
absent). `discover!` runs only the probes in the runtime's language:

- an `:sh` runtime runs the sh probes (`grammar.probe/probes`, then the PHP
  and Perl modules' probes);
- a `:python` runtime (`python3 -c`) runs `grammar.probe/py-probes`:
  `:py/sys`, `:py/stdout-buffer` (NUL, UTF-8 and CR LF through
  `sys.stdout.buffer`), `:py/pathlib`, `:py/subprocess`, `:py/urllib`,
  `:py.os/mkdir`, `:py.os/unlink`, and `:interp/sh` (subprocess runs
  `["sh" "-c" ...]` with `sh` found on PATH).

Cap names are shared. Every python probe except `:interp/sh` is also lifted
into an sh probe that runs the same script as `python3 -c '<script>'`, so
`:py/pathlib` proved by a python runtime and `:py/pathlib` proved by a shell
of the python3 it reaches are one law. `:interp/sh` has its own probe per
language. A python runtime never probes the inner shell's grammar
(`:sh/printf`, quoting), so delegating `:python` to `:sh` plans as
`:blocked-on-unknown` there unless those are declared.

A runtime whose top-level language has no probes (`:php`, `:perl`: their
modules probe through sh) is refused by `discover!` with an ex-info naming
the languages that do have probes.

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

Gated real relay labs (offline by default):

```sh
docker build -t grammar-relay-lab:1 test/lab/sshd
GRAMMAR_SSHD=1 clojure -M:test -n grammar.ssh-relay-test

# three VirtualBox VMs; the key is the one passed to vagrant up
(cd test/lab/vagrant && LAB_PUBKEY="$(cat k.pub)" LAB_PRIVKEY="$(base64 -w0 k)" vagrant up)
GRAMMAR_VAGRANT=1 GRAMMAR_VAGRANT_KEY=$PWD/k clojure -M:test -n grammar.vagrant-relay-test
(cd test/lab/vagrant && vagrant destroy -f)
```

## Relay hops

A hop (`grammar.context`) runs the inner program somewhere else: `env -i`,
`sudo -n -- sh -c`, `ssh -- DEST W`, `ssh -J J1,J2 -- DEST W`, or
`printf FMT | ssh -- DEST sh`. Each re-parses the text its own way:

| hop   | parses of the inner program                                  |
|-------|--------------------------------------------------------------|
| env, sudo | outer sh (one quoted word), then `sh -c`; env/sudo exec argv |
| ssh   | outer sh, then ssh JOINS its argv with spaces and the remote LOGIN shell parses the result |
| -J    | as ssh: the jump hosts forward TCP and never parse           |
| stdin | outer sh parses an octal-ASCII printf FORMAT; the login shell parses only the word `sh`; sh reads the program from stdin |

A hop carries `:context`; inside it the solver plans against
`(get profiles context)`, the profile `discover-chain!` probed THROUGH the
hop (a `StackedRuntime`), and a context with no profile answers `:unknown`.
`:target` names where the leaf must run. The direct ssh hops need
`:sh/posix-words` of their far side, which a csh or fish login refutes;
the stdin hop reaches those shells anyway.

`grammar.parse.sh` / `grammar.parse.relay` are a pure oracle of these
parsers (POSIX sh, bash, fish, csh word splitting and quote removal, ssh's
join, sudo/env exec, python -c). Property tests check that N encodings
through it are identity for arbitrary bytes, and that the oracle agrees
with real dash, busybox ash and bash wherever it accepts a text.

## Release

Built and published with [hive-build](https://clojars.org/io.github.hive-agi/hive-build)
(`clojure -T:build <task>`; coordinates in `version.edn`, version floor in
`VERSION`).

- `ci.yml`: the suite, the licence check and a jar build on every pull request
  and every push outside `main`.
- `release.yml`: on a push to `main` that changes the library (src, test,
  deps, version, licence, workflows), runs the suite and checks the licence.
  It releases patch+1 of the highest of `VERSION`, the newest `v*` tag and the
  newest release on Clojars, and stops if Clojars cannot be asked. It deploys
  to Clojars and checks the upload, then pushes the tag and publishes a GitHub
  release with that version's changelog section and the jar. A tag this
  workflow pushes therefore names a version that shipped from that commit.
  Nothing is committed back to `main`. Secrets: `CLOJARS_USERNAME`,
  `CLOJARS_DEPLOY_TOKEN`.

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
