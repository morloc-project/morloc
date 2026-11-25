# manager-tests

Integration tests for the **mim serving path** -- the whole flow a
user follows to expose a compiled morloc program to an AI client (MCP) or an
HTTP/JSON API:

```
mim install  ->  expose add  ->  expose eval  ->  start
   ->  /call, /discover, /health, /mcp (tools/list + tools/call), /eval
   ->  mim eval / status / stop
```

These are shell integration tests in the style of `daemon-tests`, `mcp-tests`,
and `stress`. They intentionally live **outside** the Haskell golden/unit suite
so the main test suite carries no dependency on `mim` or a container
engine.

## Layout

| File          | Purpose                                                         |
|---------------|----------------------------------------------------------------|
| `run-tests.sh`| The suite (grouped; see below).                                |
| `demo.loc`    | Tiny module (`module demo`) exporting `greet` and `add`.        |
| `demo.py`     | Python source backing the two exports.                         |
| `mcp_http.py` | Minimal MCP-over-HTTP client (initialize/list/call over `POST /mcp`). The stdio client in `mcp-tests/` does not fit the front-end's HTTP transport. |

## Groups

`./run-tests.sh [group...]` -- no args runs every group whose prerequisites are
met; name groups to filter (`./run-tests.sh help expose`).

| Group    | What it checks                                                    | Needs |
|----------|------------------------------------------------------------------|-------|
| `help`   | `mim -h` lists `install`/`expose`/`eval`/`start`/`status`/`stop`; every subcommand `-h` parses; `expose -h` lists `add`/`rm`/`list`/`eval`. | binary only |
| `expose` | The declarative exposure state machine: `expose add/rm/list/eval`, the not-installed rejection, idempotent add, per-protocol (mcp[] vs api[]) sets, eval as an independent capability. Run against a **sandboxed** `XDG_CONFIG_HOME`/`XDG_DATA_HOME`, so no engine or real environment is touched. | binary only |
| `serve`  | Full end to end against the **active** environment: `install` -> `expose add --as mcp,api` -> `expose eval` -> `start`, then `/call` (positional), `/discover` + `/discover/<module>` (positional-arg help), `/health`, CORS preflight, MCP `tools/list` + `tools/call` (named args, `demo__greet`), `/eval` + `mim eval` + the MCP `eval` tool, `status`, and `stop`. | engine + active env |
| `auth`   | A `start --auth-token` serve returns 401 without the bearer and 200 with it, on both adapters; `/health` stays open (liveness needs no token). | engine + active env |

The `serve` and `auth` groups **skip** (not fail) when a container engine, an
active morloc environment, `curl`, or `python3` is missing, so the suite is
safe to run anywhere.

## Prerequisites

- `help`/`expose`: just the `mim` binary. It is resolved from
  `$MORLOC_MANAGER`, then `PATH`, then the local cargo build under
  `data/rust/target/{debug,release}/mim`.
- `serve`/`auth`: additionally a container engine (docker/podman/apptainer), a
  **default** morloc environment (the first `mim new` sets it, or
  `mim update --env <env> --set-default`), `curl`, and
  `python3`. The serving code must be current -- run `morloc init -f` after
  changing the nexus/runtime.

## Notes

- The install identity is the **module name** (`demo`), independent of the file
  name or `-o`; the serve adapters address it as `demo` and MCP tools are named
  `demo__greet` / `demo__add`.
- Serve ports default to 9765 (`serve`) and 9766 (`auth`); override with
  `MORLOC_TEST_PORT` / `MORLOC_TEST_AUTH_PORT`.
- `install` builds in this directory; `demo-build/` is git-ignored and removed
  on exit. The test module is unexposed on exit but left installed (there is no
  uninstall verb; an unexposed module is inert).
