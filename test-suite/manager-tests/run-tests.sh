#!/usr/bin/env bash
# run-tests.sh - integration tests for the morloc-manager serving path.
#
# Covers the whole path a user follows to expose a compiled program to an AI
# client or HTTP API:
#
#   morloc-manager install  ->  expose add  ->  expose eval  ->  start
#     ->  /call, /discover, /health, /mcp (tools/list + tools/call), /eval
#     ->  morloc-manager eval / status / stop
#
# The suite is grouped so it degrades gracefully:
#
#   help    - CLI surface: `morloc-manager -h` lists install/expose/eval and the
#             subcommand help pages parse. Needs only the manager binary.
#   expose  - the declarative exposure state machine (expose add/rm/list/eval),
#             run against a SANDBOXED config/data root (XDG_*), so no container
#             engine or real environment is touched. Needs only the binary.
#   serve   - full end to end: install -> expose -> start -> call/mcp/discover
#             -> eval -> stop, against the ACTIVE environment. Needs a container
#             engine AND a working active morloc environment; SKIPs otherwise.
#   auth    - a token-protected serve returns 401 without the bearer, 200 with.
#             Same prerequisites as `serve`.
#
# Usage: ./run-tests.sh [group...]
#   No args runs every group whose prerequisites are met.
#   Filter by name:  ./run-tests.sh help expose
#
# The manager binary is found via $MORLOC_MANAGER, then PATH (`morloc-manager`),
# then the local cargo debug/release build. A rebuilt nexus is required for the
# serve/auth groups: run `morloc init -f` after changing the serving code.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MCP_HTTP="$SCRIPT_DIR/mcp_http.py"

PASSED=0
FAILED=0
SKIPPED=0
TOTAL=0
FAILURES=()
WORK_DIRS=()

# The env/module the serve groups create. `demo` is the MODULE name in demo.loc
# and therefore the install identity and the serve address.
MODULE="demo"
SERVE_PORT="${MORLOC_TEST_PORT:-9765}"
AUTH_PORT="${MORLOC_TEST_AUTH_PORT:-9766}"
AUTH_TOKEN="mgr-test-token"
STARTED_SERVE=0
STARTED_AUTH=0

if [[ -t 1 ]]; then
    GREEN=$'\033[32m' RED=$'\033[31m' YELLOW=$'\033[33m' BOLD=$'\033[1m' RESET=$'\033[0m'
else
    GREEN='' RED='' YELLOW='' BOLD='' RESET=''
fi

# ---------------------------------------------------------------------------
# Manager binary resolution
# ---------------------------------------------------------------------------
resolve_manager() {
    if [[ -n "${MORLOC_MANAGER:-}" && -x "${MORLOC_MANAGER}" ]]; then
        echo "$MORLOC_MANAGER"; return 0
    fi
    if command -v morloc-manager >/dev/null 2>&1; then
        command -v morloc-manager; return 0
    fi
    local root="$SCRIPT_DIR/../../data/rust/target"
    for cand in "$root/debug/morloc-manager" "$root/release/morloc-manager"; do
        [[ -x "$cand" ]] && { echo "$cand"; return 0; }
    done
    return 1
}

MANAGER="$(resolve_manager || true)"

# ---------------------------------------------------------------------------
# Assertions
# ---------------------------------------------------------------------------
pass() { printf "  %-58s %sPASS%s\n" "$1" "$GREEN" "$RESET"; PASSED=$((PASSED+1)); TOTAL=$((TOTAL+1)); }
fail() {
    printf "  %-58s %sFAIL%s\n" "$1" "$RED" "$RESET"
    FAILED=$((FAILED+1)); TOTAL=$((TOTAL+1)); FAILURES+=("$1")
    [[ -n "${2:-}" ]] && echo "      expected: $2"
    [[ -n "${3:-}" ]] && echo "      actual:   $3"
}
skip() { printf "  %-58s %sSKIP%s\n" "$1" "$YELLOW" "$RESET"; SKIPPED=$((SKIPPED+1)); TOTAL=$((TOTAL+1)); }

assert_eq() { if [[ "$3" == "$2" ]]; then pass "$1"; else fail "$1" "$2" "$3"; fi; }
assert_contains() { if grep -qF -- "$2" <<<"$3"; then pass "$1"; else fail "$1" "contains: $2" "$(head -c 300 <<<"$3")"; fi; }
assert_not_contains() { if grep -qF -- "$2" <<<"$3"; then fail "$1" "absent: $2" "$(head -c 300 <<<"$3")"; else pass "$1"; fi; }
assert_ok() { if [[ "$2" -eq 0 ]]; then pass "$1"; else fail "$1" "exit 0" "exit $2"; fi; }
assert_fail() { if [[ "$2" -ne 0 ]]; then pass "$1"; else fail "$1" "nonzero exit" "exit 0"; fi; }

# HTTP GET/POST returning "<status>\n<body>". `curl` is required for the
# serve/auth groups; the http group check gates on it.
http_get() { curl -s -o - -w $'\n%{http_code}' "$@"; }

cleanup() {
    if [[ "$STARTED_SERVE" -eq 1 && -n "$MANAGER" ]]; then
        "$MANAGER" stop >/dev/null 2>&1 || true
    fi
    if [[ "$STARTED_AUTH" -eq 1 && -n "$MANAGER" ]]; then
        "$MANAGER" stop >/dev/null 2>&1 || true
    fi
    # Remove the test module from the exposure set (leaves it installed; there
    # is no uninstall verb, but an unexposed module is inert).
    [[ -n "$MANAGER" ]] && "$MANAGER" expose rm "$MODULE" >/dev/null 2>&1 || true
    # `install` builds in the mounted working directory; drop the artifacts.
    rm -rf "$SCRIPT_DIR/${MODULE}-build" 2>/dev/null || true
    for d in "${WORK_DIRS[@]}"; do rm -rf "$d"; done
}
trap cleanup EXIT

section() { printf "\n%s== %s ==%s\n" "$BOLD" "$1" "$RESET"; }

# ---------------------------------------------------------------------------
# help - CLI surface (needs only the binary)
# ---------------------------------------------------------------------------
group_help() {
    section "help"
    if [[ -z "$MANAGER" ]]; then skip "manager binary not found"; return; fi

    local h; h="$("$MANAGER" -h 2>&1)"
    # Every serving subcommand must be discoverable from the top-level help.
    for sub in install expose start eval status stop; do
        assert_contains "top-level -h lists '$sub'" "$sub" "$h"
    done
    # Subcommand help pages must parse (clap exits 0 on -h).
    for sub in install expose start eval status stop; do
        "$MANAGER" "$sub" -h >/dev/null 2>&1
        assert_ok "'$sub -h' parses" $?
    done
    # `expose` sub-actions.
    local eh; eh="$("$MANAGER" expose -h 2>&1)"
    for act in add rm list eval; do
        assert_contains "expose -h lists '$act'" "$act" "$eh"
    done
}

# ---------------------------------------------------------------------------
# expose - declarative state machine, fully sandboxed (no engine, no container)
# ---------------------------------------------------------------------------
group_expose() {
    section "expose (sandboxed state machine)"
    if [[ -z "$MANAGER" ]]; then skip "manager binary not found"; return; fi

    local sb; sb="$(mktemp -d "${TMPDIR:-/tmp}/mgr-expose.XXXXXX")"; WORK_DIRS+=("$sb")
    local env_name="mgr-test-env"
    export XDG_CONFIG_HOME="$sb/config"
    export XDG_DATA_HOME="$sb/data"
    local cfg_dir="$XDG_CONFIG_HOME/morloc/environments/$env_name"
    local data_dir="$XDG_DATA_HOME/morloc/environments/$env_name"
    local expose_yaml="$cfg_dir/expose.yaml"
    mkdir -p "$cfg_dir" "$data_dir/bin"
    # Minimal env config so resolve/find succeed (engine unused by `expose`).
    cat >"$cfg_dir/env.yaml" <<EOF
name: $env_name
base_image: docker.io/library/busybox:latest
engine: podman
EOF
    # Two "installed" modules (the launcher presence is what expose validates).
    : >"$data_dir/bin/$MODULE"
    : >"$data_dir/bin/other"

    local rc
    "$MANAGER" expose add "$MODULE" --as mcp --env "$env_name" >/dev/null 2>&1; rc=$?
    assert_ok "expose add demo --as mcp" $rc
    assert_contains "expose.yaml records demo under mcp" "$MODULE" "$(cat "$expose_yaml" 2>/dev/null)"

    # Not-installed module is rejected up front.
    "$MANAGER" expose add ghost --as mcp --env "$env_name" >/dev/null 2>&1; rc=$?
    assert_fail "expose add of a non-installed module errors" $rc

    # Per-protocol: a module can be API-only (mcp[] and api[] are separate sets).
    "$MANAGER" expose add other --as api --env "$env_name" >/dev/null 2>&1
    local listed; listed="$("$MANAGER" expose list --env "$env_name" 2>&1)"
    assert_contains "expose list shows demo" "$MODULE" "$listed"
    assert_contains "expose list shows other" "other" "$listed"

    # Idempotent add (same module+protocol twice does not error/duplicate).
    "$MANAGER" expose add "$MODULE" --as mcp --env "$env_name" >/dev/null 2>&1
    assert_ok "expose add is idempotent" $?

    # eval is an independent capability with its own allow-list.
    "$MANAGER" expose eval --allow "$MODULE" --env "$env_name" >/dev/null 2>&1
    assert_contains "expose eval --allow records the allow-list" "$MODULE" "$(cat "$expose_yaml" 2>/dev/null)"
    "$MANAGER" expose eval --off --env "$env_name" >/dev/null 2>&1
    assert_ok "expose eval --off" $?

    # Removal clears the module from every set.
    "$MANAGER" expose rm "$MODULE" --env "$env_name" >/dev/null 2>&1
    local after; after="$("$MANAGER" expose list --env "$env_name" 2>&1)"
    assert_not_contains "expose rm removes demo from list" "$MODULE" "$after"

    unset XDG_CONFIG_HOME XDG_DATA_HOME
}

# ---------------------------------------------------------------------------
# Prerequisite detection for the container-backed groups
# ---------------------------------------------------------------------------
have_engine() {
    for e in docker podman apptainer singularity; do
        command -v "$e" >/dev/null 2>&1 && return 0
    done
    return 1
}

serve_prereqs_ok() {
    [[ -n "$MANAGER" ]] || { echo "manager binary not found"; return 1; }
    command -v curl >/dev/null 2>&1 || { echo "curl not found"; return 1; }
    command -v python3 >/dev/null 2>&1 || { echo "python3 not found"; return 1; }
    have_engine || { echo "no container engine"; return 1; }
    # An active environment must exist (install/start run inside it).
    "$MANAGER" info >/dev/null 2>&1 || { echo "no active morloc environment"; return 1; }
    return 0
}

wait_for_health() { # url
    local i
    for i in $(seq 1 60); do
        if curl -sf "$1/health" >/dev/null 2>&1; then return 0; fi
        sleep 1
    done
    return 1
}

# ---------------------------------------------------------------------------
# serve - full end to end against the active environment
# ---------------------------------------------------------------------------
group_serve() {
    section "serve (install -> expose -> start -> call/mcp/discover -> eval)"
    local why
    if ! why="$(serve_prereqs_ok)"; then skip "serve prerequisites: $why"; return; fi

    cd "$SCRIPT_DIR" || { fail "cd to test dir"; return; }

    # install (module identity = `demo`, independent of the file name)
    "$MANAGER" install demo.loc >/tmp/mgr-install.log 2>&1
    if [[ $? -ne 0 ]]; then
        fail "morloc-manager install demo.loc" "exit 0" "$(tail -c 400 /tmp/mgr-install.log)"
        skip "remaining serve asserts (install failed)"; return
    fi
    pass "morloc-manager install demo.loc"

    "$MANAGER" expose add "$MODULE" --as mcp,api >/dev/null 2>&1
    assert_ok "expose add demo --as mcp,api" $?
    "$MANAGER" expose eval --allow "$MODULE" >/dev/null 2>&1
    assert_ok "expose eval --allow demo" $?

    "$MANAGER" start -p "$SERVE_PORT:$SERVE_PORT" >/tmp/mgr-start.log 2>&1
    if [[ $? -ne 0 ]]; then
        fail "morloc-manager start" "exit 0" "$(tail -c 400 /tmp/mgr-start.log)"
        skip "remaining serve asserts (start failed)"; return
    fi
    STARTED_SERVE=1
    pass "morloc-manager start"

    local url="http://127.0.0.1:$SERVE_PORT"
    if ! wait_for_health "$url"; then
        fail "serve /health became ready" "200 within 60s" "$(tail -c 300 /tmp/mgr-start.log)"
        skip "remaining serve asserts (never healthy)"; return
    fi
    pass "serve /health became ready"

    # status reports the running serve
    local st; st="$("$MANAGER" status 2>&1)"
    assert_contains "status lists the serve" "$SERVE_PORT" "$st"

    # --- API adapter: /call is positional ---
    local body
    body="$(curl -s -X POST -H 'Content-Type: application/json' -d '["world"]' "$url/call/$MODULE/greet")"
    assert_contains "POST /call/demo/greet returns the greeting" "Hello, world!" "$body"
    body="$(curl -s -X POST -H 'Content-Type: application/json' -d '[2,3]' "$url/call/$MODULE/add")"
    assert_contains "POST /call/demo/add returns the sum" "5" "$body"
    # A module not exposed on the API is 404.
    body="$(curl -s -X POST -d '[]' "$url/call/nope/greet")"
    assert_contains "POST /call to an unexposed module 404s" "not exposed" "$body"

    # --- discovery (API side) ---
    local disc; disc="$(curl -s "$url/discover")"
    assert_contains "/discover lists the api module" "$MODULE" "$disc"
    assert_contains "/discover points MCP clients at tools/list" "tools/list" "$disc"
    local dmod; dmod="$(curl -s "$url/discover/$MODULE")"
    assert_contains "/discover/demo describes the greet command" "greet" "$dmod"
    assert_contains "/discover/demo describes the add command" "add" "$dmod"
    local dnope; dnope="$(http_get "$url/discover/nope")"
    assert_contains "/discover of an unexposed module 404s" "not exposed" "$dnope"

    # --- CORS preflight (before auth, no token needed) ---
    local pf; pf="$(curl -s -i -X OPTIONS "$url/mcp")"
    assert_contains "OPTIONS preflight returns 204" "204" "$pf"
    assert_contains "OPTIONS preflight sets Allow-Methods" "Access-Control-Allow-Methods" "$pf"
    local corsget; corsget="$(curl -s -i "$url/discover")"
    assert_contains "responses carry Allow-Origin" "Access-Control-Allow-Origin" "$corsget"

    # --- MCP adapter: named args, namespaced tools ---
    local tools; tools="$(python3 "$MCP_HTTP" list --url "$url/mcp")"
    assert_contains "tools/list exposes demo__greet" "${MODULE}__greet" "$tools"
    assert_contains "tools/list exposes demo__add" "${MODULE}__add" "$tools"
    assert_contains "tools/list exposes the eval tool" "eval" "$tools"
    local tcall; tcall="$(python3 "$MCP_HTTP" call --url "$url/mcp" --tool "${MODULE}__greet" --args '{"name":"mcp"}')"
    assert_contains "tools/call demo__greet round-trips" "Hello, mcp!" "$tcall"

    # --- eval (API route, CLI, and MCP tool) ---
    local evalexpr='import demo (add)
add 1.0 2.0'
    local ev; ev="$(curl -s -X POST -H 'Content-Type: application/json' \
        --data "$(python3 -c 'import json,sys; print(json.dumps({"expr": sys.stdin.read()}))' <<<"$evalexpr")" \
        "$url/eval")"
    assert_contains "POST /eval evaluates an allowed expression" "3" "$ev"
    local evtool; evtool="$(python3 "$MCP_HTTP" call --url "$url/mcp" --tool eval \
        --args "$(python3 -c 'import json,sys; print(json.dumps({"expression": sys.stdin.read()}))' <<<"$evalexpr")")"
    assert_contains "MCP eval tool evaluates an allowed expression" "3" "$evtool"
    local evcli; evcli="$("$MANAGER" eval -p "$SERVE_PORT" "$evalexpr" 2>&1)"
    assert_contains "morloc-manager eval reaches the serve" "3" "$evcli"

    # --- teardown ---
    "$MANAGER" stop >/dev/null 2>&1
    assert_ok "morloc-manager stop" $?
    STARTED_SERVE=0
    sleep 1
    curl -sf "$url/health" >/dev/null 2>&1
    assert_fail "serve endpoint is down after stop" $?
}

# ---------------------------------------------------------------------------
# auth - a token-protected serve gates both adapters
# ---------------------------------------------------------------------------
group_auth() {
    section "auth (token-gated serve)"
    local why
    if ! why="$(serve_prereqs_ok)"; then skip "auth prerequisites: $why"; return; fi

    cd "$SCRIPT_DIR" || { fail "cd to test dir"; return; }
    # Reuse the installed+exposed module from `serve`; install/expose are
    # idempotent so this group also stands alone.
    "$MANAGER" install demo.loc >/dev/null 2>&1
    "$MANAGER" expose add "$MODULE" --as mcp,api >/dev/null 2>&1

    "$MANAGER" start -p "$AUTH_PORT:$AUTH_PORT" --auth-token "$AUTH_TOKEN" >/tmp/mgr-auth.log 2>&1
    if [[ $? -ne 0 ]]; then
        fail "start --auth-token" "exit 0" "$(tail -c 400 /tmp/mgr-auth.log)"
        skip "remaining auth asserts (start failed)"; return
    fi
    STARTED_AUTH=1
    pass "start --auth-token"

    local url="http://127.0.0.1:$AUTH_PORT"
    if ! wait_for_health "$url"; then
        fail "auth serve /health ready" "200 within 60s" "$(tail -c 300 /tmp/mgr-auth.log)"
        skip "remaining auth asserts (never healthy)"; return
    fi
    pass "auth serve /health ready (no token required for liveness)"

    local code
    code="$(curl -s -o /dev/null -w '%{http_code}' "$url/discover")"
    assert_eq "/discover without token -> 401" "401" "$code"
    code="$(curl -s -o /dev/null -w '%{http_code}' -H "Authorization: Bearer $AUTH_TOKEN" "$url/discover")"
    assert_eq "/discover with token -> 200" "200" "$code"
    code="$(curl -s -o /dev/null -w '%{http_code}' -X POST -d '["x"]' "$url/call/$MODULE/greet")"
    assert_eq "/call without token -> 401" "401" "$code"

    "$MANAGER" stop >/dev/null 2>&1
    STARTED_AUTH=0
}

# ---------------------------------------------------------------------------
# Driver
# ---------------------------------------------------------------------------
RUN_GROUPS=("$@")
if [[ ${#RUN_GROUPS[@]} -eq 0 ]]; then RUN_GROUPS=(help expose serve auth); fi

for g in "${RUN_GROUPS[@]}"; do
    case "$g" in
        help)   group_help ;;
        expose) group_expose ;;
        serve)  group_serve ;;
        auth)   group_auth ;;
        *) echo "unknown group: $g (known: help expose serve auth)" ;;
    esac
done

echo
printf "%s%d passed, %d failed, %d skipped, %d total%s\n" \
    "$BOLD" "$PASSED" "$FAILED" "$SKIPPED" "$TOTAL" "$RESET"
if [[ ${#FAILURES[@]} -gt 0 ]]; then
    echo "Failures:"
    for f in "${FAILURES[@]}"; do echo "  - $f"; done
fi
[[ "$FAILED" -eq 0 ]]
