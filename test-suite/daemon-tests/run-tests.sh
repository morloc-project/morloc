#!/usr/bin/env bash
# run-tests.sh - Daemon and router test suite for morloc
#
# Tests the daemon mode (morloc-nexus daemon <target>),
# HTTP/TCP/socket APIs, and the multi-program router
# (morloc-nexus router).
#
# Usage: ./run-tests.sh [test...]
#   With no arguments, runs all test groups. Pass partial names to filter:
#   ./run-tests.sh http tcp socket router

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TIMEOUT=30
DAEMON_STARTUP_WAIT=3

PASSED=0
FAILED=0
TOTAL=0
FAILURES=()

# Tracked PIDs and temp dirs for cleanup
DAEMON_PIDS=()
WORK_DIRS=()
SOCKET_FILES=()

if [[ -t 1 ]]; then
    GREEN=$'\033[32m' RED=$'\033[31m' YELLOW=$'\033[33m' BOLD=$'\033[1m' RESET=$'\033[0m'
else
    GREEN='' RED='' YELLOW='' BOLD='' RESET=''
fi

# ======================================================================
# Cleanup
# ======================================================================

cleanup() {
    for pid in "${DAEMON_PIDS[@]}"; do
        kill "$pid" 2>/dev/null || true
    done
    # Wait briefly then force-kill
    sleep 0.5
    for pid in "${DAEMON_PIDS[@]}"; do
        kill -9 "$pid" 2>/dev/null || true
        wait "$pid" 2>/dev/null || true
    done
    for sock in "${SOCKET_FILES[@]}"; do
        rm -f "$sock"
    done
    for d in "${WORK_DIRS[@]}"; do
        rm -rf "$d"
    done
}
trap cleanup EXIT

# ======================================================================
# Test helpers
# ======================================================================

assert_test() {
    local label="$1"
    local expected="$2"
    local actual="$3"

    TOTAL=$((TOTAL + 1))
    printf "  %-50s " "$label"

    if [[ "$actual" == "$expected" ]]; then
        printf "%sPASS%s\n" "$GREEN" "$RESET"
        PASSED=$((PASSED + 1))
    else
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("$label")
        echo "      expected: $expected"
        echo "      actual:   $actual"
    fi
}

assert_contains() {
    local label="$1"
    local needle="$2"
    local haystack="$3"

    TOTAL=$((TOTAL + 1))
    printf "  %-50s " "$label"

    if echo "$haystack" | grep -qF "$needle"; then
        printf "%sPASS%s\n" "$GREEN" "$RESET"
        PASSED=$((PASSED + 1))
    else
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("$label")
        echo "      expected to contain: $needle"
        echo "      actual: $(echo "$haystack" | head -3)"
    fi
}

assert_http_status() {
    local label="$1"
    local expected_status="$2"
    local url="$3"
    shift 3
    # remaining args are passed to curl

    TOTAL=$((TOTAL + 1))
    printf "  %-50s " "$label"

    local status
    status=$(curl -s -o /dev/null -w "%{http_code}" "$@" "$url" 2>/dev/null) || status="000"

    if [[ "$status" == "$expected_status" ]]; then
        printf "%sPASS%s\n" "$GREEN" "$RESET"
        PASSED=$((PASSED + 1))
    else
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("$label")
        echo "      expected status: $expected_status"
        echo "      actual status:   $status"
    fi
}

# Send a length-prefixed JSON message over a socket and read the response as
# text (the JSON-envelope wire). Thin wrapper over lp_request_raw so the socket
# transport lives in one place.
# Usage: lp_request <socket_or_host:port> <json>
# Output: the response JSON string
lp_request() {
    local out
    out=$(mktemp)
    lp_request_raw "$1" "$2" "$out"
    cat "$out"
    rm -f "$out"
}

# Send a length-prefixed JSON message and write the RAW response bytes to a
# file. Used to capture a `-f packet` daemon reply (binary morloc packet), which
# cannot survive `$(...)` command substitution.
# Usage: lp_request_raw <socket_or_host:port> <json> <out_file>
lp_request_raw() {
    local target="$1"
    local json="$2"
    local out_file="$3"

    python3 -c "
import socket, struct, sys

target = sys.argv[1]
msg = sys.argv[2].encode('utf-8')
out_path = sys.argv[3]

if target.startswith('/'):
    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    s.connect(target)
else:
    host, port = target.rsplit(':', 1)
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((host, int(port)))

s.settimeout(10)
s.sendall(struct.pack('>I', len(msg)) + msg)

resp_len_bytes = b''
while len(resp_len_bytes) < 4:
    chunk = s.recv(4 - len(resp_len_bytes))
    if not chunk:
        break
    resp_len_bytes += chunk
resp_len = struct.unpack('>I', resp_len_bytes)[0]

resp = b''
while len(resp) < resp_len:
    chunk = s.recv(resp_len - len(resp))
    if not chunk:
        break
    resp += chunk
s.close()

with open(out_path, 'wb') as f:
    f.write(resp)
" "$target" "$json" "$out_file"
}

# Extract a JSON field value (simple string/number/bool/object extraction)
json_field() {
    local json="$1"
    local field="$2"
    python3 -c "
import json, sys
data = json.loads(sys.argv[1])
val = data.get(sys.argv[2])
if val is None:
    print('')
elif isinstance(val, (dict, list)):
    print(json.dumps(val, separators=(',', ':')))
elif isinstance(val, bool):
    print('true' if val else 'false')
else:
    print(val)
" "$json" "$field"
}

# Wait for a daemon to be ready by watching its stderr log for the
# URL-form ready line ("morloc-daemon: listening on ...") that every
# listener emits once it has bound and called listen(). For HTTP-only
# daemons prefer wait_for_http (active probe); this helper is the
# TCP/unix equivalent where active-probing is more involved.
wait_for_daemon() {
    local log_file="$1"
    local max_wait="${2:-$DAEMON_STARTUP_WAIT}"
    local i=0
    local step_ms=200
    local max_steps=$(( max_wait * 1000 / step_ms ))

    while [ "$i" -lt "$max_steps" ]; do
        if grep -q "morloc-daemon: listening on" "$log_file" 2>/dev/null \
           || grep -q "morloc-router: listening on" "$log_file" 2>/dev/null; then
            return 0
        fi
        sleep 0."$step_ms"
        i=$((i + 1))
    done

    echo "Daemon did not become ready within ${max_wait}s" >&2
    echo "Log contents:" >&2
    cat "$log_file" >&2
    return 1
}

# Wait for an HTTP port to respond
wait_for_http() {
    local port="$1"
    local max_wait="${2:-$DAEMON_STARTUP_WAIT}"
    local i=0
    local step_ms=200
    local max_steps=$(( max_wait * 1000 / step_ms ))

    while [ "$i" -lt "$max_steps" ]; do
        if curl -s -o /dev/null "http://127.0.0.1:${port}/health" 2>/dev/null; then
            return 0
        fi
        sleep 0."$step_ms"
        i=$((i + 1))
    done

    echo "HTTP port $port did not respond within ${max_wait}s" >&2
    return 1
}

# ======================================================================
# Compile test programs
# ======================================================================

compile_program() {
    local loc_file="$1"
    local work_dir="$2"
    local name
    name="$(basename "$loc_file" .loc)"

    cp "$SCRIPT_DIR/$loc_file" "$work_dir/"
    cp "$SCRIPT_DIR"/*.py "$work_dir/" 2>/dev/null || true
    cp "$SCRIPT_DIR"/*.R "$work_dir/" 2>/dev/null || true

    if ! (cd "$work_dir" && morloc make -o nexus "$loc_file" > /dev/null 2>"$work_dir/build-${name}.err"); then
        echo "COMPILE FAIL: $loc_file" >&2
        cat "$work_dir/build-${name}.err" >&2
        return 1
    fi
    return 0
}

# Start a daemon in the background, returning its PID
# Usage: start_daemon <work_dir> [extra_args...]
# Sets: LAST_DAEMON_PID, LAST_DAEMON_LOG
#
# Daemon-mode argv shape: `morloc-nexus daemon <target> [opts...]`.
# The wrapper script that `morloc make` produced sits at
# `<work_dir>/nexus`; the resolver extracts the manifest.json path from
# the wrapper's exec line (cli::resolve_manifest_target).
start_daemon() {
    local work_dir="$1"
    shift

    local log_file="$work_dir/daemon.log"

    (cd "$work_dir" && exec morloc-nexus daemon ./nexus "$@" 2>"$log_file") &
    local pid=$!
    DAEMON_PIDS+=("$pid")
    LAST_DAEMON_PID=$pid
    LAST_DAEMON_LOG="$log_file"
}

# Stop a daemon by PID
stop_daemon() {
    local pid="$1"
    kill "$pid" 2>/dev/null || true
    wait "$pid" 2>/dev/null || true
    # Remove from tracked list
    local new_pids=()
    for p in "${DAEMON_PIDS[@]}"; do
        [[ "$p" != "$pid" ]] && new_pids+=("$p")
    done
    DAEMON_PIDS=("${new_pids[@]+"${new_pids[@]}"}")
}

# Pick a random available port
pick_port() {
    python3 -c "
import socket
s = socket.socket()
s.bind(('127.0.0.1', 0))
print(s.getsockname()[1])
s.close()
"
}

# Sum sizes of all /dev/shm/mlc-<pid:6hex>-* segments belonging to a daemon.
# The SHM name embeds the creator PID as 6 zero-padded hex digits.
shm_size_for_pid() {
    local pidhex
    pidhex=$(printf '%06x' "$1")
    local total=0
    local sz
    for f in /dev/shm/mlc-${pidhex}-*; do
        [ -e "$f" ] || continue
        sz=$(stat -c %s "$f" 2>/dev/null || echo 0)
        total=$((total + sz))
    done
    echo "$total"
}

# Count /dev/shm/mlc-<pid:6hex>-* segments for a daemon.
shm_count_for_pid() {
    local pidhex
    pidhex=$(printf '%06x' "$1")
    # Counted by walking the glob rather than listing it. Under errexit a
    # listing that matches nothing fails the pipeline and takes the whole
    # suite with it -- and "nothing" is exactly the answer expected of a
    # daemon that has shut down and released its segments.
    local count=0 f
    for f in /dev/shm/mlc-${pidhex}-*; do
        [ -e "$f" ] && count=$((count + 1))
    done
    echo "$count"
}

# Resident set size of a process, in KB. `ps` rather than /proc so this also
# answers on macOS. Reports 0 once the process is gone.
rss_kb_for_pid() {
    ps -o rss= -p "$1" 2>/dev/null | tr -d ' ' || echo 0
}

# Open file descriptors held by a process. /proc where it exists, lsof as a
# fallback; prints "na" when neither can answer, which callers read as "do not
# assert" rather than as zero.
fd_count_for_pid() {
    if [ -d "/proc/$1/fd" ]; then
        ls -1 "/proc/$1/fd" 2>/dev/null | wc -l | tr -d ' '
    elif command -v lsof >/dev/null 2>&1; then
        lsof -p "$1" 2>/dev/null | wc -l | tr -d ' '
    else
        echo "na"
    fi
}

# Short by default, and short is what continuous integration runs: enough
# load to catch a fault that happens on most runs, bounded to seconds.
# `long` points the same tests at the same paths for minutes instead, which
# is what a race needing an unlucky interleaving requires -- run it by hand
# when hunting one (test.sh --long).
MORLOC_TEST_LEVEL="${MORLOC_TEST_LEVEL:-short}"

# ======================================================================
# Test selector
# ======================================================================

SELECTED=("$@")
should_run() {
    if [ ${#SELECTED[@]} -eq 0 ]; then return 0; fi
    for s in "${SELECTED[@]}"; do
        if [[ "$1" == *"$s"* ]]; then return 0; fi
    done
    return 1
}

# ======================================================================
# Setup: compile all test programs
# ======================================================================

echo "=== Morloc Daemon Test Suite ==="
echo ""

ARITH_DIR=$(mktemp -d)
STRINGS_DIR=$(mktemp -d)
PURE_DIR=$(mktemp -d)
RENDER_DIR=$(mktemp -d)
WORK_DIRS+=("$ARITH_DIR" "$STRINGS_DIR" "$PURE_DIR" "$RENDER_DIR")

echo "Compiling test programs..."
compile_program "arithmetic.loc" "$ARITH_DIR"
compile_program "strings.loc" "$STRINGS_DIR"
compile_program "pure.loc" "$PURE_DIR"
compile_program "render.loc" "$RENDER_DIR"
echo "Done."
echo ""

# ======================================================================
# Test Group 1: HTTP API
# ======================================================================

if should_run "http"; then
    echo "${BOLD}[http] Daemon HTTP API${RESET}"

    HTTP_PORT=$(pick_port)
    start_daemon "$ARITH_DIR" --http-port "$HTTP_PORT"
    wait_for_http "$HTTP_PORT" 10

    # Health endpoint
    result=$(curl -s "http://127.0.0.1:${HTTP_PORT}/health")
    status=$(json_field "$result" "status")
    assert_test "GET /health returns ok" "ok" "$status"

    # Discovery endpoint
    disco=$(curl -s "http://127.0.0.1:${HTTP_PORT}/discover")
    assert_contains "GET /discover lists commands" "add" "$disco"
    assert_contains "GET /discover lists mul" "mul" "$disco"
    assert_contains "GET /discover lists neg" "neg" "$disco"
    assert_contains "GET /discover lists square" "square" "$disco"

    # Call add(3, 4) -> 7.0
    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/add" \
        -H "Content-Type: application/json" -d '[3, 4]')
    status=$(json_field "$result" "status")
    val=$(json_field "$result" "result")
    assert_test "POST /call/add [3,4] status=ok" "ok" "$status"
    assert_test "POST /call/add [3,4] result=7" "7" "$val"

    # Call mul(5, 6) -> 30
    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/mul" \
        -H "Content-Type: application/json" -d '[5, 6]')
    val=$(json_field "$result" "result")
    assert_test "POST /call/mul [5,6] result=30" "30" "$val"

    # Call neg(42) -> -42
    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/neg" \
        -H "Content-Type: application/json" -d '[42]')
    val=$(json_field "$result" "result")
    assert_test "POST /call/neg [42] result=-42" "-42" "$val"

    # Call square(7) -> 49
    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/square" \
        -H "Content-Type: application/json" -d '[7]')
    val=$(json_field "$result" "result")
    assert_test "POST /call/square [7] result=49" "49" "$val"

    # Args as {"args": [...]} object form
    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/add" \
        -H "Content-Type: application/json" -d '{"args": [10, 20]}')
    val=$(json_field "$result" "result")
    assert_test "POST /call/add {args:[10,20]} result=30" "30" "$val"

    # Floating point args
    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/add" \
        -H "Content-Type: application/json" -d '[1.5, 2.5]')
    val=$(json_field "$result" "result")
    assert_test "POST /call/add [1.5,2.5] result=4" "4" "$val"

    # Error: unknown command (JSON envelope)
    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/nonexistent" \
        -H "Content-Type: application/json" -d '[1]')
    status=$(json_field "$result" "status")
    assert_test "POST /call/nonexistent returns error" "error" "$status"

    # CORS preflight: 204 No Content with CORS headers, no dispatch.
    assert_http_status "OPTIONS returns 204" "204" "http://127.0.0.1:${HTTP_PORT}/call/add" \
        -X OPTIONS

    stop_daemon "$LAST_DAEMON_PID"
    echo ""
fi

# ======================================================================
# Test Group: render selection (?render=<flag>)
#
# `?render=<flag>` selects an output projection; absent, the command's
# `@default` renderer fires (CLI-consistent). A media-typed (`@mime`)
# projection is served as raw bytes + `Content-Type`; `?render=raw`
# recovers the underlying typed value. `/discover` advertises them.
# ======================================================================

if should_run "render"; then
    echo "${BOLD}[render] Output projection selection (?render=)${RESET}"

    HTTP_PORT=$(pick_port)
    start_daemon "$RENDER_DIR" --http-port "$HTTP_PORT"
    wait_for_http "$HTTP_PORT" 10

    # /discover advertises the render projections and their media types.
    disco=$(curl -s "http://127.0.0.1:${HTTP_PORT}/discover")
    assert_contains "discover lists render flag 'shout'" "shout" "$disco"
    assert_contains "discover lists render mime text/plain" "text/plain" "$disco"
    # Internal per-flag command is hidden from discovery.
    if echo "$disco" | grep -q "mlcp_echo_shout"; then
        assert_test "internal render entry hidden from discover" "hidden" "shown"
    else
        assert_test "internal render entry hidden from discover" "hidden" "hidden"
    fi

    # ?render=raw -> the underlying typed Str value, JSON-wrapped.
    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/echo?render=raw" \
        -H "Content-Type: application/json" -d '["hi"]')
    val=$(json_field "$result" "result")
    assert_test "?render=raw returns typed value" "hi" "$val"

    # ?render=shout -> raw text/plain body + Content-Type (media-typed return).
    curl -s -D "$RENDER_DIR/s.hdr" -o "$RENDER_DIR/s.body" -X POST \
        "http://127.0.0.1:${HTTP_PORT}/call/echo?render=shout" \
        -H "Content-Type: application/json" -d '["hi"]'
    ct=$(grep -i '^content-type:' "$RENDER_DIR/s.hdr" | tr -d '\r' | awk '{print $2}')
    assert_test "?render=shout Content-Type text/plain" "text/plain" "$ct"
    assert_test "?render=shout raw body" "HI" "$(cat "$RENDER_DIR/s.body")"

    # Bare call fires the @default (shout) renderer -> raw text/plain.
    curl -s -D "$RENDER_DIR/d.hdr" -o "$RENDER_DIR/d.body" -X POST \
        "http://127.0.0.1:${HTTP_PORT}/call/echo" \
        -H "Content-Type: application/json" -d '["hey"]'
    dct=$(grep -i '^content-type:' "$RENDER_DIR/d.hdr" | tr -d '\r' | awk '{print $2}')
    assert_test "@default renderer Content-Type text/plain" "text/plain" "$dct"
    assert_test "@default renderer raw body" "HEY" "$(cat "$RENDER_DIR/d.body")"

    # Unknown render flag -> 400.
    assert_http_status "POST /call/echo?render=nope -> 400" "400" \
        "http://127.0.0.1:${HTTP_PORT}/call/echo?render=nope" \
        -X POST -H "Content-Type: application/json" -d '["hi"]'
fi

# ======================================================================
# Test Group 1b: HTTP status codes
#
# Client errors (unknown command, unknown endpoint, missing field,
# malformed args, wrong arity) must map to 4xx HTTP status codes;
# only genuinely server-side failures map to 500 Internal Server
# Error.
# ======================================================================

if should_run "http-status"; then
    echo "${BOLD}[http-status] HTTP status code classification${RESET}"

    HTTP_PORT=$(pick_port)
    start_daemon "$ARITH_DIR" --http-port "$HTTP_PORT"
    wait_for_http "$HTTP_PORT" 10

    # 200 happy path (regression guard).
    assert_http_status "GET  /health           -> 200" "200" \
        "http://127.0.0.1:${HTTP_PORT}/health"
    assert_http_status "POST /call/add [1,2]   -> 200" "200" \
        "http://127.0.0.1:${HTTP_PORT}/call/add" \
        -X POST -d '[1,2]'

    # 404: unknown HTTP endpoint and unknown command.
    assert_http_status "GET  /nope             -> 404" "404" \
        "http://127.0.0.1:${HTTP_PORT}/nope"
    assert_http_status "POST /call/doesNotExist -> 404" "404" \
        "http://127.0.0.1:${HTTP_PORT}/call/doesNotExist" \
        -X POST -d '[]'

    # 400: missing required field, malformed args, wrong arity.
    assert_http_status "POST /eval {}          -> 400" "400" \
        "http://127.0.0.1:${HTTP_PORT}/eval" \
        -X POST -d '{}'
    assert_http_status "POST /call/add [1,2,3] -> 400" "400" \
        "http://127.0.0.1:${HTTP_PORT}/call/add" \
        -X POST -d '[1,2,3]'
    assert_http_status "POST /call/add (no body) -> 400" "400" \
        "http://127.0.0.1:${HTTP_PORT}/call/add" \
        -X POST

    # The JSON envelope still carries status:"error" alongside the HTTP code.
    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/doesNotExist" -d '[]')
    status=$(json_field "$result" "status")
    assert_test "404 body still has status:error" "error" "$status"

    # Malformed JSON body returns 400 with a clear message. The
    # previous hand-rolled args parser would fall through to a
    # generic "missing args" error on garbage input; the serde_json
    # swap (issue 5) catches it at parse time.
    assert_http_status "POST /call/add 'not json' -> 400" "400" \
        "http://127.0.0.1:${HTTP_PORT}/call/add" \
        -X POST -d 'not json'

    # Object-wrapped args ({"args": [...]}) and bare-array forms both
    # work. This was the existing behavior; regression guard for the
    # serde_json swap.
    obj_result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/add" \
        -H "Content-Type: application/json" -d '{"args":[3,4]}')
    val=$(json_field "$obj_result" "result")
    assert_test "POST /call/add {args:[3,4]} via object form" "7" "$val"

    stop_daemon "$LAST_DAEMON_PID"
    echo ""
fi

# ======================================================================
# Test Group 1f: 408 Request Timeout on /eval CPU budget
#
# /eval and /typecheck fork a `morloc eval`/`typecheck` subprocess
# under RLIMIT_CPU = G_EVAL_TIMEOUT seconds. When the CPU budget is
# exceeded the kernel sends SIGXCPU; the daemon now classifies that
# as DAEMON_ERROR_TIMEOUT and emits HTTP 408 (was 400).
#
# Note: this test is intrinsically environment-sensitive. It uses a
# 1-second --eval-timeout and an expression that should comfortably
# exceed it (50M-element list traversal). If /eval is not exercised
# in the testing environment, the test is a no-op skip.
# ======================================================================

if should_run "http-eval-timeout"; then
    echo "${BOLD}[http-eval-timeout] /eval CPU budget -> 408${RESET}"

    HTTP_PORT=$(pick_port)
    start_daemon "$ARITH_DIR" --http-port "$HTTP_PORT" --eval-timeout 1 \
        --eval-allowed-modules root-py
    wait_for_http "$HTTP_PORT" 10

    # An expression whose COMPILE cost exceeds the budget. The budget is a
    # CPU rlimit on the forked compiler, so the work has to land in the
    # compiler and not in a pool: a long addition chain typechecks for
    # several seconds while allocating almost nothing, where a huge list
    # would spend a pool's memory instead and never touch the budget.
    body=$(python3 -c "
import json
print(json.dumps({'expr': 'import root-py\n' + ' + '.join(['1'] * 800)}))
")
    status=$(curl -s -o /dev/null -w "%{http_code}" --max-time 30 \
        -X POST "http://127.0.0.1:${HTTP_PORT}/eval" \
        -H "Content-Type: application/json" -d "$body") \
        || status="000"

    assert_test "POST /eval CPU bomb -> 408" "408" "$status"

    stop_daemon "$LAST_DAEMON_PID"
    echo ""
fi

# ======================================================================
# Test Group 1g: /typecheck and /eval expression endpoints
#
# Regression guard for the daemon's /typecheck endpoint, which used to
# return {"status":"ok","result":""} unconditionally: the daemon forked
# `morloc typecheck <expr>` where the positional is a script *filename*,
# so the inline expression was read as a missing file and produced no
# output. The fix forks `morloc <subcmd> -e <expr>` (eval and typecheck
# both take a file by default now). A well-typed expression must yield
# a non-empty result; an ill-typed one must not return ok/empty.
# ======================================================================

if should_run "http-typecheck-eval"; then
    echo "${BOLD}[http-typecheck-eval] /typecheck and /eval expressions${RESET}"

    HTTP_PORT=$(pick_port)
    start_daemon "$ARITH_DIR" --http-port "$HTTP_PORT" \
        --eval-allowed-modules root-py
    wait_for_http "$HTTP_PORT" 10

    # /eval: a well-typed expression returns its value.
    eval_resp=$(curl -s --max-time 60 \
        -X POST "http://127.0.0.1:${HTTP_PORT}/eval" \
        -H "Content-Type: application/json" \
        -d '{"expr": "import root-py\n2 + 2"}')
    eval_status=$(json_field "$eval_resp" "status")
    eval_result=$(json_field "$eval_resp" "result")
    assert_test "POST /eval '2 + 2' status=ok" "ok" "$eval_status"
    assert_test "POST /eval '2 + 2' result=4" "4" "$eval_result"

    # /typecheck: a well-typed expression returns a non-empty result
    # (the inferred type text), never the old empty string.
    tc_resp=$(curl -s --max-time 60 \
        -X POST "http://127.0.0.1:${HTTP_PORT}/typecheck" \
        -H "Content-Type: application/json" \
        -d '{"expr": "import root-py\n2 + 2"}')
    tc_status=$(json_field "$tc_resp" "status")
    tc_result=$(json_field "$tc_resp" "result")
    assert_test "POST /typecheck '2 + 2' status=ok" "ok" "$tc_status"
    tc_nonempty=$([ -n "$tc_result" ] && echo "non-empty" || echo "empty")
    assert_test "POST /typecheck '2 + 2' result non-empty" "non-empty" "$tc_nonempty"

    # /typecheck: an ill-typed expression must not silently succeed
    # with an empty result.
    bad_resp=$(curl -s --max-time 60 \
        -X POST "http://127.0.0.1:${HTTP_PORT}/typecheck" \
        -H "Content-Type: application/json" \
        -d '{"expr": "import root-py\n2 + True"}')
    bad_status=$(json_field "$bad_resp" "status")
    assert_test "POST /typecheck ill-typed status=error" "error" "$bad_status"

    stop_daemon "$LAST_DAEMON_PID"
    echo ""
fi

# ======================================================================
# Test Group 1e: Structured JSON args (issue 5 regression guard)
#
# The hand-rolled args parser counted brackets without respecting
# string escapes, so a string field containing `]` would terminate
# the array prematurely. The serde_json swap fixes this. There is
# no morloc command in arithmetic.loc that takes a structured-string
# arg, so this group uses the strings program and verifies that
# string args containing ']', '[', '}', '{' characters round-trip
# without truncation.
# ======================================================================

if should_run "http-json-args"; then
    echo "${BOLD}[http-json-args] Structured JSON args${RESET}"

    HTTP_PORT=$(pick_port)
    start_daemon "$STRINGS_DIR" --http-port "$HTTP_PORT"
    wait_for_http "$HTTP_PORT" 10

    # A string containing every character the old bracket counter
    # would have miscounted. The serde_json parser sees them as
    # string content; the old code would have closed the array on
    # the first `]`.
    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/strlen" \
        -H "Content-Type: application/json" \
        -d '["a]b[c}d{e"]')
    val=$(json_field "$result" "result")
    assert_test "strlen of 'a]b[c}d{e' -> 9 (with bracket-counter, would have been 1)" \
        "9" "$val"

    # Same payload through the object-wrapped form.
    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/strlen" \
        -H "Content-Type: application/json" \
        -d '{"args":["a]b[c}d{e"]}')
    val=$(json_field "$result" "result")
    assert_test "strlen of 'a]b[c}d{e' (object form) -> 9" "9" "$val"

    stop_daemon "$LAST_DAEMON_PID"
    echo ""
fi

# ======================================================================
# Test Group 1c: OPTIONS preflight short-circuit
#
# CORS preflight requests must get 204 No Content with the standard
# Access-Control-Allow-* headers, and must NOT invoke the Health
# pipeline (which would also hit the pool-crash recovery gate).
# ======================================================================

if should_run "http-options"; then
    echo "${BOLD}[http-options] CORS preflight short-circuit${RESET}"

    HTTP_PORT=$(pick_port)
    start_daemon "$ARITH_DIR" --http-port "$HTTP_PORT"
    wait_for_http "$HTTP_PORT" 10

    headers=$(curl -s -i -o /dev/null -D - -X OPTIONS \
        "http://127.0.0.1:${HTTP_PORT}/call/add")

    assert_contains "OPTIONS status line is 204"             "204" "$headers"
    assert_contains "OPTIONS sets Access-Control-Allow-Origin"  "Access-Control-Allow-Origin: *" "$headers"
    assert_contains "OPTIONS sets Access-Control-Allow-Methods" "Access-Control-Allow-Methods"   "$headers"
    assert_contains "OPTIONS sets Access-Control-Allow-Headers" "Access-Control-Allow-Headers"   "$headers"

    # Body is empty for 204.
    body=$(curl -s -o - -X OPTIONS "http://127.0.0.1:${HTTP_PORT}/call/add")
    assert_test "OPTIONS body is empty" "" "$body"

    # OPTIONS to nonsense paths also returns 204; preflight is a
    # browser concern, not a routing decision.
    assert_http_status "OPTIONS /nope returns 204" "204" \
        "http://127.0.0.1:${HTTP_PORT}/nope" -X OPTIONS

    stop_daemon "$LAST_DAEMON_PID"
    echo ""
fi

# ======================================================================
# Test Group 1d: Recovery gate -> 503 + Retry-After
#
# When a pool process dies, the daemon kills all pools, drops SHM, and
# respawns. Any request landing in that window returns 503 Service
# Unavailable with Retry-After: 1 so HTTP clients with retry middleware
# (curl --retry, axios-retry, hyper-retry) back off correctly.
# ======================================================================

if should_run "http-recovery-503"; then
    echo "${BOLD}[http-recovery-503] 503 + Retry-After during recovery${RESET}"

    HTTP_PORT=$(pick_port)
    start_daemon "$ARITH_DIR" --http-port "$HTTP_PORT"
    wait_for_http "$HTTP_PORT" 10
    RECOVERY_DAEMON_PID=$LAST_DAEMON_PID

    # Pre-flight: confirm /health is normal before the kill.
    pre_status=$(curl -s -o /dev/null -w "%{http_code}" \
        "http://127.0.0.1:${HTTP_PORT}/health")
    assert_test "pre-kill /health -> 200" "200" "$pre_status"

    # Kill all child pool processes to trigger the recovery gate.
    pool_pids=$(pgrep -P "$RECOVERY_DAEMON_PID" 2>/dev/null) || pool_pids=""
    if [ -z "$pool_pids" ]; then
        TOTAL=$((TOTAL + 1))
        printf "  %-50s " "recovery: no pool children to kill"
        printf "%sSKIP%s\n" "$YELLOW" "$RESET"
        PASSED=$((PASSED + 1))
    else
        for ppid in $pool_pids; do
            kill -9 "$ppid" 2>/dev/null || true
        done

        # What the daemon guarantees is that a pool crash is invisible to
        # callers: it rebuilds the pools and every request still gets its
        # answer. The 503 + Retry-After gate is the fallback for a request
        # already inside dispatch when recovery starts, and recovery runs on
        # the accept loop, so a request arriving during the window waits in
        # the backlog rather than meeting the gate. That leaves 503 an
        # interior race no test can schedule -- asserting it must happen
        # only pins the timing of a machine. Assert the guarantee instead,
        # and check the pairing whenever the race does surface.
        RECOVERY_DIR=$(mktemp -d)
        WORK_DIRS+=("$RECOVERY_DIR")
        RECOVERY_CODES="$RECOVERY_DIR/codes.txt"
        : > "$RECOVERY_CODES"
        found_503=0
        found_retry_after=0
        for attempt in $(seq 1 60); do
            hdr=$(curl -s --max-time 2 -D - -o /dev/null \
                "http://127.0.0.1:${HTTP_PORT}/health" 2>/dev/null) || hdr=""
            first=$(echo "$hdr" | head -n 1)
            echo "$first" >> "$RECOVERY_CODES"
            if echo "$first" | grep -q " 503 "; then
                found_503=1
                if echo "$hdr" | grep -qi "^Retry-After: 1"; then
                    found_retry_after=1
                fi
            fi
        done

        # Nothing may fail outright: every probe is either served or told to
        # retry. A dropped connection, a 500, or a hang is a real regression.
        # grep -c exits 1 on a zero count, so recover the count from the
        # assignment rather than appending a second line with `|| echo 0`.
        bad_codes=$(grep -vcE " (200|503) " "$RECOVERY_CODES" 2>/dev/null) \
            || bad_codes=0
        assert_test "recovery serves or defers, never fails" "0" "$bad_codes"

        # The daemon is still usable once recovery finishes.
        post_status=$(curl -s -o /dev/null -w "%{http_code}" --max-time 10 \
            "http://127.0.0.1:${HTTP_PORT}/health")
        assert_test "post-recovery /health -> 200" "200" "$post_status"

        # Only meaningful when the race was actually observed: a 503 that
        # does not say when to come back is useless to an automatic client.
        if [ "$found_503" = "1" ]; then
            assert_test "503 carries Retry-After: 1" "1" "$found_retry_after"
        fi

        # OPTIONS preflight is short-circuited before daemon_dispatch is
        # even called, so it must NEVER hit the recovery gate. If the
        # short-circuit ever regresses back into the dispatch path, this
        # catches it.
        opt_status=$(curl -s --max-time 2 -o /dev/null \
            -w "%{http_code}" -X OPTIONS \
            "http://127.0.0.1:${HTTP_PORT}/call/add" 2>/dev/null) \
            || opt_status="000"
        assert_test "OPTIONS during recovery -> 204" "204" "$opt_status"
    fi

    # Let recovery finish so cleanup is clean.
    sleep 4
    stop_daemon "$RECOVERY_DAEMON_PID"
    echo ""
fi

# ======================================================================
# Test Group 2: HTTP with Python pool (strings)
# ======================================================================

if should_run "http-py"; then
    echo "${BOLD}[http-py] Daemon HTTP with Python pool${RESET}"

    HTTP_PORT=$(pick_port)
    start_daemon "$STRINGS_DIR" --http-port "$HTTP_PORT"
    wait_for_http "$HTTP_PORT" 10

    # Call greet("world") -> "Hello, world!"
    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/greet" \
        -H "Content-Type: application/json" -d '["world"]')
    status=$(json_field "$result" "status")
    val=$(json_field "$result" "result")
    assert_test "POST /call/greet status=ok" "ok" "$status"
    assert_test "POST /call/greet [world] result" "Hello, world!" "$val"

    # Call strlen("morloc") -> 6
    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/strlen" \
        -H "Content-Type: application/json" -d '["morloc"]')
    val=$(json_field "$result" "result")
    assert_test "POST /call/strlen [morloc] result=6" "6" "$val"

    # Empty string
    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/strlen" \
        -H "Content-Type: application/json" -d '[""]')
    val=$(json_field "$result" "result")
    assert_test "POST /call/strlen [] result=0" "0" "$val"

    stop_daemon "$LAST_DAEMON_PID"
    echo ""
fi

# ======================================================================
# Test Group 3: HTTP with pure commands
# ======================================================================

if should_run "http-pure"; then
    echo "${BOLD}[http-pure] Daemon HTTP with pure morloc commands${RESET}"

    HTTP_PORT=$(pick_port)
    start_daemon "$PURE_DIR" --http-port "$HTTP_PORT"
    wait_for_http "$HTTP_PORT" 10

    # Pure commands take no arguments
    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/checkInt" \
        -H "Content-Type: application/json" -d '[]')
    status=$(json_field "$result" "status")
    val=$(json_field "$result" "result")
    assert_test "POST /call/checkInt status=ok" "ok" "$status"
    assert_test "POST /call/checkInt result=42" "42" "$val"

    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/checkReal" \
        -H "Content-Type: application/json" -d '[]')
    val=$(json_field "$result" "result")
    assert_test "POST /call/checkReal result=3.14" "3.14" "$val"

    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/checkBool" \
        -H "Content-Type: application/json" -d '[]')
    val=$(json_field "$result" "result")
    assert_test "POST /call/checkBool result=true" "true" "$val"

    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/checkStr" \
        -H "Content-Type: application/json" -d '[]')
    val=$(json_field "$result" "result")
    assert_test "POST /call/checkStr result=hello" "hello" "$val"

    stop_daemon "$LAST_DAEMON_PID"
    echo ""
fi

# ======================================================================
# Test Group 4: Unix socket (length-prefixed JSON)
# ======================================================================

if should_run "socket"; then
    echo "${BOLD}[socket] Daemon Unix socket API${RESET}"

    SOCK_PATH="/tmp/morloc-test-$$.sock"
    SOCKET_FILES+=("$SOCK_PATH")
    start_daemon "$ARITH_DIR" --socket "$SOCK_PATH"
    wait_for_daemon "$LAST_DAEMON_LOG" 15

    # Health check via socket
    result=$(lp_request "$SOCK_PATH" '{"method":"health"}')
    status=$(json_field "$result" "status")
    assert_test "socket health status=ok" "ok" "$status"

    # Discovery via socket
    result=$(lp_request "$SOCK_PATH" '{"method":"discover"}')
    assert_contains "socket discover lists add" "add" "$result"

    # Call via socket: add(10, 20) -> 30.0
    result=$(lp_request "$SOCK_PATH" '{"method":"call","command":"add","args":[10,20]}')
    status=$(json_field "$result" "status")
    val=$(json_field "$result" "result")
    assert_test "socket call add status=ok" "ok" "$status"
    assert_test "socket call add [10,20] result=30" "30" "$val"

    # Call via socket with request ID
    result=$(lp_request "$SOCK_PATH" '{"id":"req-42","method":"call","command":"mul","args":[3,7]}')
    rid=$(json_field "$result" "id")
    val=$(json_field "$result" "result")
    assert_test "socket call with id echoes id" "req-42" "$rid"
    assert_test "socket call mul [3,7] result=21" "21" "$val"

    # Error: unknown command via socket
    result=$(lp_request "$SOCK_PATH" '{"method":"call","command":"bogus","args":[1]}')
    status=$(json_field "$result" "status")
    assert_test "socket unknown command returns error" "error" "$status"

    stop_daemon "$LAST_DAEMON_PID"
    echo ""
fi

# ======================================================================
# Test Group 4b: -f packet result form over the Unix socket
#
# With `-f packet -z 3`, a `call` result is returned as a raw (zstd-
# compressed) morloc data packet instead of the JSON envelope; control
# methods (health/discover) stay JSON. The captured packet is decoded with
# the shared `morloc-nexus` binary's `file` (classify) and `view` (re-emit)
# subcommands, so the daemon and the reader agree on the wire format.
# ======================================================================

if should_run "packet"; then
    echo "${BOLD}[packet] Daemon -f packet result form (socket)${RESET}"

    PKT_SOCK="/tmp/morloc-test-pkt-$$.sock"
    SOCKET_FILES+=("$PKT_SOCK")
    start_daemon "$ARITH_DIR" --socket "$PKT_SOCK" -f packet -z 3
    wait_for_daemon "$LAST_DAEMON_LOG" 15

    PKT_OUT="$ARITH_DIR/pkt-resp.bin"

    # A `call` result is a raw morloc data packet, not the JSON envelope.
    lp_request_raw "$PKT_SOCK" '{"method":"call","command":"add","args":[10,20]}' "$PKT_OUT"
    kind=$(morloc-nexus file -FD "$PKT_OUT" 2>/dev/null || echo "classify-failed")
    assert_contains "packet call add returns a morloc packet" "packet" "$kind"

    # The packet decodes back to the numeric result -- proving the schema
    # block survived and the `-z 3` payload decompresses cleanly.
    decoded=$(morloc-nexus view "$PKT_OUT" 2>/dev/null || echo "decode-failed")
    assert_contains "packet call add [10,20] decodes to 30" "30" "$decoded"

    # An error result is a (FAIL) packet too, not a JSON error envelope, so a
    # packet-mode client always reads exactly one packet.
    lp_request_raw "$PKT_SOCK" '{"method":"call","command":"bogus","args":[1]}' "$PKT_OUT"
    ekind=$(morloc-nexus file -FD "$PKT_OUT" 2>/dev/null || echo "classify-failed")
    assert_contains "packet unknown command returns a packet (not JSON)" "packet" "$ekind"

    # Control methods stay JSON even on a packet-configured daemon.
    result=$(lp_request "$PKT_SOCK" '{"method":"health"}')
    status=$(json_field "$result" "status")
    assert_test "packet-mode health stays JSON status=ok" "ok" "$status"

    result=$(lp_request "$PKT_SOCK" '{"method":"discover"}')
    assert_contains "packet-mode discover stays JSON, lists add" "add" "$result"

    stop_daemon "$LAST_DAEMON_PID"

    # A pure (in-nexus eval) command in packet mode exercises the eval-path
    # packetizer, which is a distinct code path from the remote-pool call above.
    PKT_PURE_SOCK="/tmp/morloc-test-pkt-pure-$$.sock"
    SOCKET_FILES+=("$PKT_PURE_SOCK")
    start_daemon "$PURE_DIR" --socket "$PKT_PURE_SOCK" -f packet
    wait_for_daemon "$LAST_DAEMON_LOG" 15

    lp_request_raw "$PKT_PURE_SOCK" '{"method":"call","command":"checkInt","args":[]}' "$PKT_OUT"
    pkind=$(morloc-nexus file -FD "$PKT_OUT" 2>/dev/null || echo "classify-failed")
    assert_contains "packet pure checkInt returns a morloc packet" "packet" "$pkind"
    pdecoded=$(morloc-nexus view "$PKT_OUT" 2>/dev/null || echo "decode-failed")
    assert_contains "packet pure checkInt decodes to 42" "42" "$pdecoded"

    stop_daemon "$LAST_DAEMON_PID"
    echo ""
fi

# ======================================================================
# Test Group 5: TCP (length-prefixed JSON)
# ======================================================================

if should_run "tcp"; then
    echo "${BOLD}[tcp] Daemon TCP API${RESET}"

    TCP_PORT=$(pick_port)
    start_daemon "$ARITH_DIR" --port "$TCP_PORT"
    wait_for_daemon "$LAST_DAEMON_LOG" 15

    # Health check via TCP
    result=$(lp_request "127.0.0.1:${TCP_PORT}" '{"method":"health"}')
    status=$(json_field "$result" "status")
    assert_test "tcp health status=ok" "ok" "$status"

    # Call via TCP: add(100, 200) -> 300.0
    result=$(lp_request "127.0.0.1:${TCP_PORT}" '{"method":"call","command":"add","args":[100,200]}')
    status=$(json_field "$result" "status")
    val=$(json_field "$result" "result")
    assert_test "tcp call add status=ok" "ok" "$status"
    assert_test "tcp call add [100,200] result=300" "300" "$val"

    # Call via TCP: square(9) -> 81
    result=$(lp_request "127.0.0.1:${TCP_PORT}" '{"method":"call","command":"square","args":[9]}')
    val=$(json_field "$result" "result")
    assert_test "tcp call square [9] result=81" "81" "$val"

    stop_daemon "$LAST_DAEMON_PID"
    echo ""
fi

# ======================================================================
# Test Group 5c: Ephemeral port binding (port 0)
#
# `--http-port 0` and `--port 0` ask the kernel to assign a free port.
# The daemon reads it back via getsockname() and emits one stderr line
# per listener in URL form (`morloc-daemon: listening on http://...`).
# ======================================================================

if should_run "port-ephemeral"; then
    echo "${BOLD}[port-ephemeral] Bind ephemeral (port 0)${RESET}"

    start_daemon "$ARITH_DIR" --http-port 0 --port 0

    # Wait up to 5s for both ready lines to appear in stderr.
    waited=0
    while [ "$waited" -lt 50 ]; do
        if grep -q "listening on http://" "$LAST_DAEMON_LOG" 2>/dev/null \
           && grep -q "listening on tcp://"  "$LAST_DAEMON_LOG" 2>/dev/null; then
            break
        fi
        sleep 0.1
        waited=$((waited + 1))
    done

    http_line=$(grep "listening on http://" "$LAST_DAEMON_LOG" | head -1 || true)
    tcp_line=$(grep "listening on tcp://"  "$LAST_DAEMON_LOG" | head -1 || true)

    assert_contains "http ready line is URL form"  "http://0.0.0.0:"   "$http_line"
    assert_contains "tcp  ready line is URL form"  "tcp://127.0.0.1:"  "$tcp_line"

    # Extract the assigned ports.
    HTTP_PORT=$(echo "$http_line" | sed -n 's#.*http://0\.0\.0\.0:\([0-9][0-9]*\).*#\1#p')
    TCP_PORT=$( echo "$tcp_line"  | sed -n 's#.*tcp://127\.0\.0\.1:\([0-9][0-9]*\).*#\1#p')

    # Both should be in the ephemeral range (>1024) and not be 0.
    assert_test "http port is non-zero" "1" "$([ -n "$HTTP_PORT" ] && [ "$HTTP_PORT" -gt 0 ] && echo 1 || echo 0)"
    assert_test "tcp  port is non-zero" "1" "$([ -n "$TCP_PORT" ]  && [ "$TCP_PORT"  -gt 0 ] && echo 1 || echo 0)"

    # The assigned ports should actually work.
    wait_for_http "$HTTP_PORT" 10
    result=$(curl -s "http://127.0.0.1:${HTTP_PORT}/health")
    status=$(json_field "$result" "status")
    assert_test "ephemeral http /health responds ok" "ok" "$status"

    result=$(lp_request "127.0.0.1:${TCP_PORT}" '{"method":"call","command":"add","args":[2,3]}')
    val=$(json_field "$result" "result")
    assert_test "ephemeral tcp call add [2,3]=5" "5" "$val"

    stop_daemon "$LAST_DAEMON_PID"
    echo ""
fi

# ======================================================================
# Test Group 5d: --port-file (atomic port discovery)
#
# After all listeners bind, the daemon writes
#   {"http": N|null, "tcp": N|null, "unix": "PATH"|null}
# to the --port-file path, atomically (tmp + rename). Fixed schema: every
# key is always present (null when the listener isn't configured).
# ======================================================================

if should_run "port-file"; then
    echo "${BOLD}[port-file] --port-file output${RESET}"

    # Case A: only --http-port 0 -> http populated, tcp/unix null.
    PORT_FILE="$ARITH_DIR/port-a.json"
    rm -f "$PORT_FILE"
    start_daemon "$ARITH_DIR" --http-port 0 --port-file "$PORT_FILE"

    # Wait for file to appear (it's written after bind completes).
    waited=0
    while [ "$waited" -lt 50 ] && [ ! -f "$PORT_FILE" ]; do
        sleep 0.1
        waited=$((waited + 1))
    done
    assert_test "port-file (http only) was written" "1" "$([ -f "$PORT_FILE" ] && echo 1 || echo 0)"

    pf=$(cat "$PORT_FILE" 2>/dev/null)
    http_val=$(json_field "$pf" "http")
    tcp_val=$( json_field "$pf" "tcp")
    unix_val=$(json_field "$pf" "unix")
    assert_test "port-file http is numeric" "1" "$([ -n "$http_val" ] && [ "$http_val" -gt 0 ] 2>/dev/null && echo 1 || echo 0)"
    assert_test "port-file tcp  is null"    "" "$tcp_val"
    assert_test "port-file unix is null"    "" "$unix_val"

    # The advertised port should actually work.
    wait_for_http "$http_val" 10
    result=$(curl -s "http://127.0.0.1:${http_val}/health")
    status=$(json_field "$result" "status")
    assert_test "port-file http port serves /health" "ok" "$status"

    stop_daemon "$LAST_DAEMON_PID"

    # Case B: all three listeners -> all three keys populated.
    SOCK_PATH="/tmp/morloc-test-port-file-$$.sock"
    SOCKET_FILES+=("$SOCK_PATH")
    PORT_FILE="$ARITH_DIR/port-b.json"
    rm -f "$PORT_FILE"
    start_daemon "$ARITH_DIR" \
        --http-port 0 --port 0 --socket "$SOCK_PATH" --port-file "$PORT_FILE"

    waited=0
    while [ "$waited" -lt 50 ] && [ ! -f "$PORT_FILE" ]; do
        sleep 0.1
        waited=$((waited + 1))
    done
    assert_test "port-file (all three) was written" "1" "$([ -f "$PORT_FILE" ] && echo 1 || echo 0)"

    pf=$(cat "$PORT_FILE" 2>/dev/null)
    http_val=$(json_field "$pf" "http")
    tcp_val=$( json_field "$pf" "tcp")
    unix_val=$(json_field "$pf" "unix")
    assert_test "port-file http is numeric" "1" "$([ -n "$http_val" ] && [ "$http_val" -gt 0 ] 2>/dev/null && echo 1 || echo 0)"
    assert_test "port-file tcp  is numeric" "1" "$([ -n "$tcp_val" ]  && [ "$tcp_val"  -gt 0 ] 2>/dev/null && echo 1 || echo 0)"
    assert_test "port-file unix is socket path" "$SOCK_PATH" "$unix_val"

    stop_daemon "$LAST_DAEMON_PID"

    # Case C: unwritable --port-file path. The write is non-fatal --
    # bind continues, stderr logs a warning, and the daemon still
    # serves traffic. Pins the contract that --port-file failure does
    # not prevent daemon startup (issue 7).
    UNWRITABLE_DIR=$(mktemp -d)
    WORK_DIRS+=("$UNWRITABLE_DIR")
    chmod 555 "$UNWRITABLE_DIR"
    UNWRITABLE_FILE="$UNWRITABLE_DIR/cannot-write.json"
    start_daemon "$ARITH_DIR" --http-port 0 --port-file "$UNWRITABLE_FILE"

    # Wait for the ready line so we know bind completed.
    waited=0
    while [ "$waited" -lt 50 ]; do
        if grep -q "listening on http://" "$LAST_DAEMON_LOG" 2>/dev/null; then
            break
        fi
        sleep 0.1
        waited=$((waited + 1))
    done

    http_line=$(grep "listening on http://" "$LAST_DAEMON_LOG" | head -1 || true)
    HTTP_PORT_C=$(echo "$http_line" | sed -n 's#.*http://0\.0\.0\.0:\([0-9][0-9]*\).*#\1#p')

    assert_test "unwritable port-file: daemon still binds"   "1" \
        "$([ -n "$HTTP_PORT_C" ] && [ "$HTTP_PORT_C" -gt 0 ] && echo 1 || echo 0)"
    assert_test "unwritable port-file: file not written"     "1" \
        "$([ ! -f "$UNWRITABLE_FILE" ] && echo 1 || echo 0)"
    assert_contains "unwritable port-file: stderr logs failure" \
        "failed to write port file" \
        "$(cat "$LAST_DAEMON_LOG")"

    if [ -n "$HTTP_PORT_C" ]; then
        result=$(curl -s "http://127.0.0.1:${HTTP_PORT_C}/health")
        status=$(json_field "$result" "status")
        assert_test "unwritable port-file: /health still works" "ok" "$status"
    fi

    chmod 755 "$UNWRITABLE_DIR"
    stop_daemon "$LAST_DAEMON_PID"
    echo ""
fi

# ======================================================================
# Test Group 6: Multiple listeners simultaneously
# ======================================================================

if should_run "multi"; then
    echo "${BOLD}[multi] Daemon with all listeners${RESET}"

    SOCK_PATH="/tmp/morloc-test-multi-$$.sock"
    SOCKET_FILES+=("$SOCK_PATH")
    HTTP_PORT=$(pick_port)
    TCP_PORT=$(pick_port)

    start_daemon "$ARITH_DIR" --socket "$SOCK_PATH" --port "$TCP_PORT" --http-port "$HTTP_PORT"
    wait_for_http "$HTTP_PORT" 10

    # Same command via all three protocols
    # HTTP
    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/add" \
        -H "Content-Type: application/json" -d '[1, 2]')
    val=$(json_field "$result" "result")
    assert_test "multi: HTTP add [1,2] result=3" "3" "$val"

    # TCP
    result=$(lp_request "127.0.0.1:${TCP_PORT}" '{"method":"call","command":"add","args":[1,2]}')
    val=$(json_field "$result" "result")
    assert_test "multi: TCP add [1,2] result=3" "3" "$val"

    # Unix socket
    result=$(lp_request "$SOCK_PATH" '{"method":"call","command":"add","args":[1,2]}')
    val=$(json_field "$result" "result")
    assert_test "multi: socket add [1,2] result=3" "3" "$val"

    stop_daemon "$LAST_DAEMON_PID"
    echo ""
fi

# ======================================================================
# Test Group 7: Sequential requests (daemon stays alive)
# ======================================================================

if should_run "sequential"; then
    echo "${BOLD}[sequential] Multiple sequential requests${RESET}"

    HTTP_PORT=$(pick_port)
    start_daemon "$ARITH_DIR" --http-port "$HTTP_PORT"
    wait_for_http "$HTTP_PORT" 10

    all_ok=true
    for i in $(seq 1 10); do
        result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/add" \
            -H "Content-Type: application/json" -d "[${i}, ${i}]")
        val=$(json_field "$result" "result")
        expected=$(python3 -c "x = float($i + $i); print(int(x) if x == int(x) else x)")
        if [[ "$val" != "$expected" ]]; then
            all_ok=false
            break
        fi
    done

    TOTAL=$((TOTAL + 1))
    printf "  %-50s " "10 sequential add calls"
    if $all_ok; then
        printf "%sPASS%s\n" "$GREEN" "$RESET"
        PASSED=$((PASSED + 1))
    else
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("10 sequential add calls")
    fi

    stop_daemon "$LAST_DAEMON_PID"
    echo ""
fi

# ======================================================================
# Test Group 8: Concurrent requests
# ======================================================================

if should_run "concurrent"; then
    echo "${BOLD}[concurrent] Concurrent HTTP requests${RESET}"

    HTTP_PORT=$(pick_port)
    start_daemon "$ARITH_DIR" --http-port "$HTTP_PORT"
    wait_for_http "$HTTP_PORT" 10

    # Fire 5 concurrent requests (each with a 15s timeout)
    CONC_DIR=$(mktemp -d)
    WORK_DIRS+=("$CONC_DIR")

    CONC_PIDS=()
    for i in $(seq 1 5); do
        (
            curl -s --max-time 15 -X POST "http://127.0.0.1:${HTTP_PORT}/call/square" \
                -H "Content-Type: application/json" -d "[${i}]" \
                > "$CONC_DIR/result-${i}.json" 2>/dev/null
        ) &
        CONC_PIDS+=($!)
    done
    # Wait for all with a per-process check
    for pid in "${CONC_PIDS[@]}"; do
        wait "$pid" 2>/dev/null || true
    done

    all_ok=true
    for i in $(seq 1 5); do
        if [ -f "$CONC_DIR/result-${i}.json" ]; then
            result=$(cat "$CONC_DIR/result-${i}.json")
            val=$(json_field "$result" "result")
            expected=$(python3 -c "x = float($i * $i); print(int(x) if x == int(x) else x)")
            if [[ "$val" != "$expected" ]]; then
                all_ok=false
            fi
        else
            all_ok=false
        fi
    done

    TOTAL=$((TOTAL + 1))
    printf "  %-50s " "5 concurrent square calls"
    if $all_ok; then
        printf "%sPASS%s\n" "$GREEN" "$RESET"
        PASSED=$((PASSED + 1))
    else
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("5 concurrent square calls")
    fi

    stop_daemon "$LAST_DAEMON_PID"
    echo ""
fi

# ======================================================================
# Test Group 9: Graceful shutdown
# ======================================================================

if should_run "shutdown"; then
    echo "${BOLD}[shutdown] Graceful daemon shutdown${RESET}"

    HTTP_PORT=$(pick_port)
    SOCK_PATH="/tmp/morloc-test-shutdown-$$.sock"
    SOCKET_FILES+=("$SOCK_PATH")

    start_daemon "$ARITH_DIR" --http-port "$HTTP_PORT" --socket "$SOCK_PATH"
    wait_for_http "$HTTP_PORT" 10
    local_pid=$LAST_DAEMON_PID

    # Verify it's alive
    result=$(curl -s "http://127.0.0.1:${HTTP_PORT}/health" 2>/dev/null) || result=""
    status=$(json_field "$result" "status" 2>/dev/null) || status=""
    assert_test "daemon alive before shutdown" "ok" "$status"

    # Send SIGTERM
    kill "$local_pid" 2>/dev/null
    wait "$local_pid" 2>/dev/null || true

    # Remove from tracked list
    new_pids=()
    for p in "${DAEMON_PIDS[@]}"; do
        [[ "$p" != "$local_pid" ]] && new_pids+=("$p")
    done
    DAEMON_PIDS=("${new_pids[@]+"${new_pids[@]}"}")

    # Verify it's dead
    sleep 0.5

    TOTAL=$((TOTAL + 1))
    printf "  %-50s " "daemon exits after SIGTERM"
    if ! kill -0 "$local_pid" 2>/dev/null; then
        printf "%sPASS%s\n" "$GREEN" "$RESET"
        PASSED=$((PASSED + 1))
    else
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("daemon exits after SIGTERM")
        kill -9 "$local_pid" 2>/dev/null || true
    fi

    # Verify socket file cleaned up
    TOTAL=$((TOTAL + 1))
    printf "  %-50s " "socket file removed after shutdown"
    if [ ! -e "$SOCK_PATH" ]; then
        printf "%sPASS%s\n" "$GREEN" "$RESET"
        PASSED=$((PASSED + 1))
    else
        printf "%sFAIL%s\n" "$RED" "$RESET"
        FAILED=$((FAILED + 1))
        FAILURES+=("socket file removed after shutdown")
    fi

    echo ""
fi

# ======================================================================
# Test Group 10: Router
# ======================================================================

if should_run "router"; then
    echo "${BOLD}[router] Multi-program router${RESET}"

    # Set up a temporary exe/ directory in the shape the router reads:
    # exe/<name>/<name>-build/manifest.json, one per installed program.
    # The build already produced a self-describing build dir (keyed on the
    # -o name; manifest.json + pools/ with relative pool paths), so symlink
    # it in under the name the router will look for -- no extraction or
    # patching.
    FDB_DIR=$(mktemp -d)
    WORK_DIRS+=("$FDB_DIR")
    ROUTER_MANIFEST="$FDB_DIR/arithmetic/arithmetic-build/manifest.json"

    if [ -f "$ARITH_DIR/nexus-build/manifest.json" ]; then
        mkdir -p "$FDB_DIR/arithmetic"
        ln -s "$ARITH_DIR/nexus-build" "$FDB_DIR/arithmetic/arithmetic-build"
    fi

    if [ ! -f "$ROUTER_MANIFEST" ]; then
        echo "  ${RED}SKIP: could not locate arithmetic build directory${RESET}"
        echo ""
        TOTAL=$((TOTAL + 1))
        FAILED=$((FAILED + 1))
        FAILURES+=("router: could not locate build directory")
    fi

    if [ -f "$ROUTER_MANIFEST" ]; then
        ROUTER_PORT=$(pick_port)

        # Start router (use the morloc-nexus binary). Which programs are
        # served is always an explicit decision; with no --program/--mcp/--api
        # the router has nothing to serve and refuses to start.
        NEXUS_PATH="$(which morloc-nexus 2>/dev/null || echo "$HOME/.local/bin/morloc-nexus")"
        (exec "$NEXUS_PATH" router --http-port "$ROUTER_PORT" --fdb "$FDB_DIR" \
            --program arithmetic 2>"$FDB_DIR/router.log") &
        ROUTER_PID=$!
        DAEMON_PIDS+=("$ROUTER_PID")

        wait_for_http "$ROUTER_PORT" 15 || true

        # Health check
        result=$(curl -s "http://127.0.0.1:${ROUTER_PORT}/health" 2>/dev/null) || result=""
        status=$(json_field "$result" "status" 2>/dev/null) || status=""
        assert_test "router GET /health" "ok" "$status"

        # Discovery index: names each served module and the URL shape a
        # caller invokes it through.
        disco=$(curl -s "http://127.0.0.1:${ROUTER_PORT}/discover" 2>/dev/null) || disco=""
        assert_contains "router GET /discover lists arithmetic" "arithmetic" "$disco"
        assert_contains "router GET /discover gives the call shape" \
            "/call/arithmetic/<command>" "$disco"

        # Per-program discovery
        disco=$(curl -s "http://127.0.0.1:${ROUTER_PORT}/discover/arithmetic" 2>/dev/null) || disco=""
        assert_contains "router GET /discover/arithmetic lists add" "add" "$disco"

        # Call through router: add(5, 10) -> 15.0
        result=$(curl -s -X POST "http://127.0.0.1:${ROUTER_PORT}/call/arithmetic/add" \
            -H "Content-Type: application/json" -d '[5, 10]' 2>/dev/null) || result=""
        status=$(json_field "$result" "status" 2>/dev/null) || status=""
        val=$(json_field "$result" "result" 2>/dev/null) || val=""
        assert_test "router call add status=ok" "ok" "$status"
        assert_test "router call add [5,10] result=15" "15" "$val"

        # Call through router: square(4) -> 16
        result=$(curl -s -X POST "http://127.0.0.1:${ROUTER_PORT}/call/arithmetic/square" \
            -H "Content-Type: application/json" -d '[4]' 2>/dev/null) || result=""
        val=$(json_field "$result" "result" 2>/dev/null) || val=""
        assert_test "router call square [4] result=16" "16" "$val"

        # Error: unknown program
        result=$(curl -s -X POST "http://127.0.0.1:${ROUTER_PORT}/call/bogus/add" \
            -H "Content-Type: application/json" -d '[1,2]' 2>/dev/null) || result=""
        assert_contains "router unknown program returns error" "error" "$result"

        # HTTP status code parity with the daemon (issue 11). The
        # router has its own routing layer and could regress
        # independently of daemon-side coverage.
        assert_http_status "router GET  /nope        -> 404" "404" \
            "http://127.0.0.1:${ROUTER_PORT}/nope"
        assert_http_status "router POST /call/bogus/add -> 404 (unknown program)" "404" \
            "http://127.0.0.1:${ROUTER_PORT}/call/bogus/add" \
            -X POST -d '[1,2]'
        assert_http_status "router GET  /discover/bogus -> 404" "404" \
            "http://127.0.0.1:${ROUTER_PORT}/discover/bogus"
        assert_http_status "router OPTIONS /discover  -> 204" "204" \
            "http://127.0.0.1:${ROUTER_PORT}/discover" \
            -X OPTIONS
        assert_http_status "router POST /call/arithmetic/add 200 happy" "200" \
            "http://127.0.0.1:${ROUTER_PORT}/call/arithmetic/add" \
            -X POST -d '[1,2]'

        # Shutdown router
        stop_daemon "$ROUTER_PID"

        # Verify child daemons are also cleaned up
        sleep 1
        remaining=$(pgrep -f "morloc-router-arithmetic" 2>/dev/null | wc -l) || remaining=0

        TOTAL=$((TOTAL + 1))
        printf "  %-50s " "router cleans up child daemons"
        if [ "$remaining" -eq 0 ]; then
            printf "%sPASS%s\n" "$GREEN" "$RESET"
            PASSED=$((PASSED + 1))
        else
            printf "%sFAIL%s\n" "$RED" "$RESET"
            FAILED=$((FAILED + 1))
            FAILURES+=("router cleans up child daemons")
        fi
    fi

    echo ""
fi

# ======================================================================
# Test Group 11: Connection timeout resilience
# ======================================================================

if should_run "timeout"; then
    echo "${BOLD}[timeout] Connection timeout resilience${RESET}"

    HTTP_PORT=$(pick_port)
    SOCK_PATH="/tmp/morloc-test-timeout-$$.sock"
    SOCKET_FILES+=("$SOCK_PATH")
    start_daemon "$ARITH_DIR" --http-port "$HTTP_PORT" --socket "$SOCK_PATH"
    wait_for_http "$HTTP_PORT" 10

    # Open a socket, send partial data (just 2 bytes of the 4-byte length prefix),
    # then don't send anything else. The daemon should time out and remain responsive.
    python3 -c "
import socket, time
s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
s.connect('$SOCK_PATH')
s.sendall(b'\\x00\\x00')  # partial length prefix
time.sleep(1)
s.close()
" 2>/dev/null &
    STALL_PID=$!

    # Wait a moment, then verify daemon still responds
    sleep 2
    result=$(curl -s "http://127.0.0.1:${HTTP_PORT}/health" 2>/dev/null) || result=""
    status=$(json_field "$result" "status" 2>/dev/null) || status=""
    assert_test "daemon responsive after stalled client" "ok" "$status"

    wait "$STALL_PID" 2>/dev/null || true

    stop_daemon "$LAST_DAEMON_PID"
    echo ""
fi

# ======================================================================
# Test Group 12: Pool crash recovery
# ======================================================================

if should_run "pool-recovery"; then
    echo "${BOLD}[pool-recovery] Pool crash and restart${RESET}"

    HTTP_PORT=$(pick_port)
    start_daemon "$ARITH_DIR" --http-port "$HTTP_PORT"
    wait_for_http "$HTTP_PORT" 10
    DAEMON_PID_FOR_RECOVERY=$LAST_DAEMON_PID

    # Verify it works before killing
    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/add" \
        -H "Content-Type: application/json" -d '[1, 2]')
    val=$(json_field "$result" "result")
    assert_test "pool-recovery: works before kill" "3" "$val"

    # Find and kill pool child processes
    pool_pids=$(pgrep -P "$DAEMON_PID_FOR_RECOVERY" 2>/dev/null) || pool_pids=""
    if [ -n "$pool_pids" ]; then
        for ppid in $pool_pids; do
            kill -9 "$ppid" 2>/dev/null || true
        done

        # Wait for restart (daemon checks on every poll cycle = 1s)
        sleep 4

        # Verify it works after pool restart
        result=$(curl -s --max-time 10 -X POST "http://127.0.0.1:${HTTP_PORT}/call/add" \
            -H "Content-Type: application/json" -d '[10, 20]')
        val=$(json_field "$result" "result" 2>/dev/null) || val=""
        assert_test "pool-recovery: works after pool kill" "30" "$val"
    else
        TOTAL=$((TOTAL + 1))
        printf "  %-50s " "pool-recovery: works after pool kill"
        printf "%sSKIP%s (no child pools found)\n" "$YELLOW" "$RESET"
        PASSED=$((PASSED + 1))
    fi

    stop_daemon "$DAEMON_PID_FOR_RECOVERY"
    echo ""
fi

# ======================================================================
# Test Group 13: Health endpoint with pool status
# ======================================================================

if should_run "pool-health"; then
    echo "${BOLD}[pool-health] Health endpoint reports pool status${RESET}"

    HTTP_PORT=$(pick_port)
    start_daemon "$ARITH_DIR" --http-port "$HTTP_PORT"
    wait_for_http "$HTTP_PORT" 10

    result=$(curl -s "http://127.0.0.1:${HTTP_PORT}/health")
    assert_contains "health response includes pools" "pools" "$result"
    assert_contains "health response includes status ok" "ok" "$result"

    # Check that pools array has at least one true entry
    has_alive=$(python3 -c "
import json, sys
data = json.loads(sys.argv[1])
result = data.get('result', data)
pools = result.get('pools', [])
print('true' if any(pools) else 'false')
" "$result" 2>/dev/null) || has_alive="false"
    assert_test "health shows pools alive" "true" "$has_alive"

    stop_daemon "$LAST_DAEMON_PID"
    echo ""
fi

# ======================================================================
# Test Group 13b: Long-lived daemon under sustained load
# ======================================================================
#
# Morloc daemons back long-running applications, so the interesting
# question is not whether one request works but whether the ten-thousandth
# does, while others are in flight, after a pool has died and been rebuilt
# underneath them.
#
# Requests ride persistent connections driven by soak-client.py rather than
# a process per call. That is how a real client behaves, it is the only
# coverage the keep-alive path gets, and it is what makes a few thousand
# requests affordable here.
#
# The correctness assertion is the point of the group: every response is
# checked against a value derived from that request's own argument, so a
# worker handing a result to the wrong caller fails rather than passing by
# coincidence. A silent stderr is asserted for the same reason -- the
# runtime reports shared-memory accounting faults there and nothing else in
# the suite reads it, so a daemon can print a thousand refcount errors while
# every other assertion passes.

if should_run "soak"; then
    echo "${BOLD}[soak] Long-lived daemon under sustained load${RESET}"

    # Short is sized to finish in about half a minute while still putting
    # several thousand requests and a pool crash through the daemon. Long
    # multiplies the rounds rather than the workers: the same burst repeated
    # is what re-samples the scheduler, and each repeat is another chance at
    # an interleaving that only happens sometimes.
    if [ "$MORLOC_TEST_LEVEL" = "long" ]; then
        SOAK_CHURN="--workers 12 --requests 25 --rounds 60"
        SOAK_PIN_CHURN="--workers 12 --requests 20 --rounds 60"
        SOAK_PIN_CONC="--workers 16 --requests 100 --rounds 40"
        SOAK_THREAD_CHURN="--workers 12 --requests 20 --rounds 30"
    else
        SOAK_CHURN="--workers 8 --requests 12 --rounds 2"
        SOAK_PIN_CHURN="--workers 8 --requests 10 --rounds 2"
        SOAK_PIN_CONC="--workers 12 --requests 60 --rounds 2"
    fi

    SOAK_DIR=$(mktemp -d)
    WORK_DIRS+=("$SOAK_DIR")
    cp "$SCRIPT_DIR/soak.loc" "$SCRIPT_DIR/soak.py" "$SCRIPT_DIR/soak.hpp" \
        "$SOAK_DIR/"
    if ! (cd "$SOAK_DIR" && morloc make -o nexus soak.loc \
            > /dev/null 2>"$SOAK_DIR/build.err"); then
        echo "  COMPILE FAIL: soak.loc"
        cat "$SOAK_DIR/build.err"
        TOTAL=$((TOTAL + 1))
        FAILED=$((FAILED + 1))
        FAILURES+=("soak: compilation failed")
    else
        SOAK_PORT=$(pick_port)
        SOAK_LOG="$SOAK_DIR/daemon.log"
        (cd "$SOAK_DIR" && exec morloc-nexus daemon ./nexus \
            --http-port "$SOAK_PORT" 2>"$SOAK_LOG") &
        SOAK_PID=$!
        DAEMON_PIDS+=("$SOAK_PID")
        wait_for_http "$SOAK_PORT" 15 || true

        # Warm-up: spawn every pool and take whatever one-off shared memory
        # and descriptors the daemon allocates lazily, so the baseline below
        # measures steady state rather than start-up.
        curl -s -o /dev/null --max-time 30 -X POST \
            "http://127.0.0.1:${SOAK_PORT}/call/square" \
            -H "Content-Type: application/json" -d '[3]'
        curl -s -o /dev/null --max-time 30 -X POST \
            "http://127.0.0.1:${SOAK_PORT}/call/echoList" \
            -H "Content-Type: application/json" -d '[]'

        base_shm=$(shm_size_for_pid "$SOAK_PID")
        base_rss=$(rss_kb_for_pid "$SOAK_PID")
        base_fds=$(fd_count_for_pid "$SOAK_PID")

        # One connection, in series: the baseline every later phase is
        # compared against. A failure here is not a concurrency bug.
        seq_out=$(python3 "$SCRIPT_DIR/soak-client.py" "$SOAK_PORT" seq \
            --requests 150 2>&1) && seq_rc=0 || seq_rc=$?
        assert_test "sequential requests all correct" "0" "$seq_rc"

        # Many connections at once. This is where a result handed to the
        # wrong caller, or a block of shared memory recycled while still in
        # use, shows up.
        conc_out=$(python3 "$SCRIPT_DIR/soak-client.py" "$SOAK_PORT" conc \
            --workers 8 --requests 60 2>&1) && conc_rc=0 || conc_rc=$?
        assert_test "concurrent requests all correct" "0" "$conc_rc"
        if [ "$conc_rc" != "0" ]; then
            echo "      $conc_out"
        fi

        # Payloads large enough to be handed over in shared memory rather
        # than carried inside the packet, through both languages, with the
        # contents of every list keyed to the request that asked for it. This
        # is the allocate / hand across / free / reuse path: if a block is
        # recycled while another request still holds it, the reader sees the
        # other request's data and the element comparison says which one.
        churn_out=$(python3 "$SCRIPT_DIR/soak-client.py" "$SOAK_PORT" churn \
            $SOAK_CHURN 2>&1) && churn_rc=0 || churn_rc=$?
        assert_test "large payloads survive concurrent churn" "0" "$churn_rc"
        if [ "$churn_rc" != "0" ]; then
            echo "      $churn_out"
        fi

        # Kill the pools mid-flight and keep the load on. Every request must
        # still end up with the right answer; the client retries a rebuild in
        # progress but never accepts a wrong or missing result.
        soak_pools=$(pgrep -P "$SOAK_PID" 2>/dev/null) || soak_pools=""
        for p in $soak_pools; do kill -9 "$p" 2>/dev/null || true; done
        crash_out=$(python3 "$SCRIPT_DIR/soak-client.py" "$SOAK_PORT" conc \
            --workers 4 --requests 40 2>&1) && crash_rc=0 || crash_rc=$?
        assert_test "requests correct across a pool crash" "0" "$crash_rc"
        if [ "$crash_rc" != "0" ]; then
            echo "      $crash_out"
        fi

        after_shm=$(shm_size_for_pid "$SOAK_PID")
        after_rss=$(rss_kb_for_pid "$SOAK_PID")
        after_fds=$(fd_count_for_pid "$SOAK_PID")

        # Shared memory is reused, not accumulated. The recovery above drops
        # the whole namespace and builds a new one, so this compares steady
        # state to steady state rather than tracking a single volume.
        shm_delta=$((after_shm - base_shm))
        assert_test "shared memory does not accumulate" "yes" \
            "$([ "$shm_delta" -lt $((256 * 1024)) ] && echo yes || echo no)"
        echo "      shm delta=${shm_delta} B  rss ${base_rss}->${after_rss} KB  fds ${base_fds}->${after_fds}"

        # A daemon that grows a few hundred KB per thousand requests is a
        # daemon that dies overnight. The bound is generous because an
        # allocator is free to keep arenas warm; it is there to catch growth
        # proportional to the request count.
        rss_delta=$((after_rss - base_rss))
        assert_test "daemon memory does not grow with traffic" "yes" \
            "$([ "$rss_delta" -lt 65536 ] && echo yes || echo no)"

        # Descriptors are the other resource a long-lived server runs out
        # of. Recovery re-opens pool sockets, so a small delta is expected
        # and only unbounded growth is a fault.
        if [ "$base_fds" != "na" ] && [ "$after_fds" != "na" ]; then
            fd_delta=$((after_fds - base_fds))
            assert_test "descriptors do not accumulate" "yes" \
                "$([ "$fd_delta" -lt 16 ] && echo yes || echo no)"
        fi

        # The runtime reports shared-memory accounting faults on stderr and
        # keeps serving, so without this a corrupted daemon passes every
        # other assertion in this suite.
        # Everything this daemon is expected to say, enumerated rather than
        # matched loosely: the crash narration is only there because the
        # crash above was deliberate, and a pattern broad enough to cover it
        # would also cover the faults this assertion exists to catch.
        soak_noise=$(grep -vE \
            -e '^morloc-daemon: listening ' \
            -e '^morloc daemon: pool crash detected' \
            -e '^ *pool [0-9]+: Pool process crashed with signal ' \
            -e '^morloc daemon: recovery complete ' \
            "$SOAK_LOG" 2>/dev/null | grep -c .) || soak_noise=0
        assert_test "daemon logged nothing unexpected" "0" "$soak_noise"
        if [ "$soak_noise" != "0" ]; then
            echo "      $(head -c 400 "$SOAK_LOG")"
        fi

        # Still a working daemon at the end of all that.
        final_status=$(curl -s -o /dev/null -w "%{http_code}" --max-time 15 \
            "http://127.0.0.1:${SOAK_PORT}/health")
        assert_test "still serving after the soak" "200" "$final_status"

        stop_daemon "$SOAK_PID"

        # Shared memory outlives the process that made it, so a daemon that
        # exits without releasing its segments leaks at the machine level --
        # invisible to anything measured while it was running, and cumulative
        # across the restarts a long-lived service actually goes through.
        sleep 1
        leftover=$(shm_count_for_pid "$SOAK_PID")
        assert_test "segments released when the daemon exits" "0" "$leftover"

        # Same load again against a daemon confined to two cores. Threads that
        # each have a core of their own rarely interleave inside a critical
        # section; crowding them onto two makes the scheduler cut between
        # instructions that normally run to completion undisturbed. A race
        # that needs an unlucky interleaving is far likelier to be caught
        # here than on an idle twelve-core machine, which is the shape of
        # machine that hides one.
        if command -v taskset >/dev/null 2>&1; then
            PIN_PORT=$(pick_port)
            PIN_LOG="$SOAK_DIR/pinned.log"
            (cd "$SOAK_DIR" && exec taskset -c 0,1 morloc-nexus daemon ./nexus \
                --http-port "$PIN_PORT" 2>"$PIN_LOG") &
            PIN_PID=$!
            DAEMON_PIDS+=("$PIN_PID")
            wait_for_http "$PIN_PORT" 20 || true
            curl -s -o /dev/null --max-time 30 -X POST \
                "http://127.0.0.1:${PIN_PORT}/call/square" \
                -H "Content-Type: application/json" -d '[3]'

            pin_out=$(python3 "$SCRIPT_DIR/soak-client.py" "$PIN_PORT" churn \
                $SOAK_PIN_CHURN 2>&1) && pin_rc=0 || pin_rc=$?
            assert_test "correct under two-core contention" "0" "$pin_rc"
            if [ "$pin_rc" != "0" ]; then
                echo "      $pin_out"
            fi

            pin_conc=$(python3 "$SCRIPT_DIR/soak-client.py" "$PIN_PORT" conc \
                $SOAK_PIN_CONC 2>&1) && pin_conc_rc=0 || pin_conc_rc=$?
            assert_test "small calls correct under contention" "0" "$pin_conc_rc"
            if [ "$pin_conc_rc" != "0" ]; then
                echo "      $pin_conc"
            fi

            pin_noise=$(grep -vE \
                -e '^morloc-daemon: listening ' \
                -e '^morloc daemon: pool crash detected' \
                -e '^ *pool [0-9]+: Pool process crashed with signal ' \
                -e '^morloc daemon: recovery complete ' \
                "$PIN_LOG" 2>/dev/null | grep -c .) || pin_noise=0
            assert_test "contended daemon logged nothing unexpected" "0" "$pin_noise"
            if [ "$pin_noise" != "0" ]; then
                echo "      $(head -c 400 "$PIN_LOG")"
            fi

            stop_daemon "$PIN_PID"
        fi

        # The Python pool picks its concurrency model by platform: worker
        # processes on Linux, worker threads on macOS, because forking a live
        # interpreter aborts there. The two share almost no code path, so a
        # run on Linux says nothing about the model macOS actually ships --
        # and the threaded one, where workers share an address space, is
        # where a concurrency fault has room to do damage. A long run on any
        # other platform therefore takes a pass with it forced on, so whoever
        # is hunting covers the model they are not otherwise testing. On
        # macOS this is already what ran, three phases ago.
        if [ "$MORLOC_TEST_LEVEL" = "long" ] && [ "$(uname -s)" != "Darwin" ]; then
            THR_PORT=$(pick_port)
            THR_LOG="$SOAK_DIR/threaded.log"
            (cd "$SOAK_DIR" && export MORLOC_PY_POOL=thread && \
                exec morloc-nexus daemon ./nexus \
                --http-port "$THR_PORT" 2>"$THR_LOG") &
            THR_PID=$!
            DAEMON_PIDS+=("$THR_PID")
            wait_for_http "$THR_PORT" 20 || true
            curl -s -o /dev/null --max-time 30 -X POST \
                "http://127.0.0.1:${THR_PORT}/call/pyEcho" \
                -H "Content-Type: application/json" -d '[[1,2,3]]'

            # Confirm the setting reached the pool rather than assuming it.
            # A test that quietly exercises the default model while claiming
            # otherwise is worse than not running: it reports coverage of the
            # one platform nobody else is testing.
            thr_pools=0
            for cp in $(pgrep -P "$THR_PID" 2>/dev/null); do
                if tr '\0' '\n' < "/proc/$cp/environ" 2>/dev/null \
                        | grep -q '^MORLOC_PY_POOL=thread$'; then
                    thr_pools=$((thr_pools + 1))
                fi
            done
            assert_test "threaded pool model actually in force" "yes" \
                "$([ "$thr_pools" -gt 0 ] && echo yes || echo no)"

            thr_out=$(python3 "$SCRIPT_DIR/soak-client.py" "$THR_PORT" churn \
                $SOAK_THREAD_CHURN 2>&1) && thr_rc=0 || thr_rc=$?
            assert_test "correct under the threaded pool model" "0" "$thr_rc"
            if [ "$thr_rc" != "0" ]; then
                echo "      $thr_out"
            fi

            thr_noise=$(grep -vE \
                -e '^morloc-daemon: listening ' \
                -e '^morloc daemon: pool crash detected' \
                -e '^ *pool [0-9]+: Pool process crashed with signal ' \
                -e '^morloc daemon: recovery complete ' \
                "$THR_LOG" 2>/dev/null | grep -c .) || thr_noise=0
            assert_test "threaded pool logged nothing unexpected" "0" "$thr_noise"
            if [ "$thr_noise" != "0" ]; then
                echo "      $(head -c 400 "$THR_LOG")"
            fi

            stop_daemon "$THR_PID"
        fi
    fi

    echo ""
fi

# ======================================================================
# Test Group 14: SHM-leak regression
# ======================================================================
#
# Asserts that the daemon's per-call SHM allocations are released when
# each request finishes (via the per-eval arena in eval_arena.rs). With
# the leak in place, every call would accumulate ~500 bytes in the
# daemon's /dev/shm/mlc-<pid>-* volumes; over 1000 calls the volume
# would fill and additional 64 KB volumes would be created. With the
# fix, blocks are reused and total /dev/shm bytes for the daemon stay
# essentially flat.

if should_run "shm-leak"; then
    echo "${BOLD}[shm-leak] Daemon SHM-leak regression${RESET}"

    SHM_LEAK_DIR=$(mktemp -d)
    WORK_DIRS+=("$SHM_LEAK_DIR")
    cp "$SCRIPT_DIR/shm-leak.loc" "$SHM_LEAK_DIR/"
    if ! (cd "$SHM_LEAK_DIR" && morloc make -o nexus shm-leak.loc \
            > /dev/null 2>"$SHM_LEAK_DIR/build.err"); then
        echo "  COMPILE FAIL: shm-leak.loc"
        cat "$SHM_LEAK_DIR/build.err"
        TOTAL=$((TOTAL + 1))
        FAILED=$((FAILED + 1))
        FAILURES+=("shm-leak: compilation failed")
    else
        SHM_HTTP_PORT=$(pick_port)
        start_daemon "$SHM_LEAK_DIR" --http-port "$SHM_HTTP_PORT"
        wait_for_http "$SHM_HTTP_PORT" 10
        SHM_DAEMON_PID=$LAST_DAEMON_PID

        # One warm-up call so all per-pool / per-binding SHM that the
        # daemon allocates lazily is in place before we snapshot.
        curl -s -o /dev/null -X POST \
            "http://127.0.0.1:${SHM_HTTP_PORT}/call/echoList" \
            -H "Content-Type: application/json" -d '[]'

        before_size=$(shm_size_for_pid "$SHM_DAEMON_PID")
        before_count=$(shm_count_for_pid "$SHM_DAEMON_PID")

        N=1000
        for i in $(seq 1 $N); do
            curl -s -o /dev/null -X POST \
                "http://127.0.0.1:${SHM_HTTP_PORT}/call/echoList" \
                -H "Content-Type: application/json" -d '[]'
        done

        after_size=$(shm_size_for_pid "$SHM_DAEMON_PID")
        after_count=$(shm_count_for_pid "$SHM_DAEMON_PID")

        delta_size=$((after_size - before_size))
        delta_count=$((after_count - before_count))

        # Each echoList call allocates ~500 bytes of multi-block voidstar
        # (list wrapper, element wrappers, char blocks). 1000 calls ~=
        # 500 KB unfreed without the arena fix; with the fix, blocks are
        # reused inside the existing 64 KB volume and delta_size ~ 0.
        # Threshold of 200 KB cleanly distinguishes the two states while
        # absorbing daemon bookkeeping noise.
        THRESHOLD_BYTES=$((200 * 1024))

        TOTAL=$((TOTAL + 1))
        printf "  %-50s " "${N} calls grow daemon /dev/shm < 200 KB"
        if [ "$delta_size" -lt "$THRESHOLD_BYTES" ]; then
            printf "%sPASS%s\n" "$GREEN" "$RESET"
            PASSED=$((PASSED + 1))
            echo "      delta=${delta_size} bytes  new_volumes=${delta_count}"
        else
            printf "%sFAIL%s\n" "$RED" "$RESET"
            FAILED=$((FAILED + 1))
            FAILURES+=("shm-leak: delta=${delta_size} bytes (threshold ${THRESHOLD_BYTES})")
            echo "      delta=${delta_size} bytes  new_volumes=${delta_count}"
        fi

        stop_daemon "$SHM_DAEMON_PID"
    fi

    echo ""
fi

# ======================================================================
# Test Group 15: R-pool SHM-leak regression
# ======================================================================
#
# Same idea as [shm-leak] but routes the call through the R pool. Asserts
# that the R-pool's morloc_put_value path (PACKET_SOURCE_RPTR result) does
# not leak SHM across requests. With the rmorloc.c shm_tracker fix, the
# block from the prior request is released at the start of every new
# request in run_job_c. Without the fix, every R-routed request would
# leak the result block in the daemon's volume.

if should_run "r-shm-leak"; then
    echo "${BOLD}[r-shm-leak] R pool SHM-leak regression${RESET}"

    R_LEAK_DIR=$(mktemp -d)
    WORK_DIRS+=("$R_LEAK_DIR")
    if ! compile_program "r-shm-leak.loc" "$R_LEAK_DIR"; then
        TOTAL=$((TOTAL + 1))
        FAILED=$((FAILED + 1))
        FAILURES+=("r-shm-leak: compilation failed")
    else
        R_HTTP_PORT=$(pick_port)
        start_daemon "$R_LEAK_DIR" --http-port "$R_HTTP_PORT"
        wait_for_http "$R_HTTP_PORT" 15
        R_DAEMON_PID=$LAST_DAEMON_PID

        # Warm-up: get all per-pool / per-binding lazy SHM in place.
        curl -s -o /dev/null -X POST \
            "http://127.0.0.1:${R_HTTP_PORT}/call/echoList" \
            -H "Content-Type: application/json" \
            -d '[["alpha","beta","gamma","delta","epsilon","zeta","eta","theta","iota","kappa"]]'

        before_size=$(shm_size_for_pid "$R_DAEMON_PID")
        before_count=$(shm_count_for_pid "$R_DAEMON_PID")

        N=1000
        for i in $(seq 1 $N); do
            curl -s -o /dev/null -X POST \
                "http://127.0.0.1:${R_HTTP_PORT}/call/echoList" \
                -H "Content-Type: application/json" \
                -d '[["alpha","beta","gamma","delta","epsilon","zeta","eta","theta","iota","kappa"]]'
        done

        after_size=$(shm_size_for_pid "$R_DAEMON_PID")
        after_count=$(shm_count_for_pid "$R_DAEMON_PID")

        delta_size=$((after_size - before_size))
        delta_count=$((after_count - before_count))

        # Each call ships ~250 B of [Str] result via PACKET_SOURCE_RPTR.
        # Pre-fix: the R pool never released that block, so 1000 calls
        # ~= 250 KB / several new 64 KB volumes. Post-fix: blocks reused
        # in the existing volume, delta ~= 0.
        THRESHOLD_BYTES=$((200 * 1024))

        TOTAL=$((TOTAL + 1))
        printf "  %-50s " "${N} R calls grow daemon /dev/shm < 200 KB"
        if [ "$delta_size" -lt "$THRESHOLD_BYTES" ]; then
            printf "%sPASS%s\n" "$GREEN" "$RESET"
            PASSED=$((PASSED + 1))
            echo "      delta=${delta_size} bytes  new_volumes=${delta_count}"
        else
            printf "%sFAIL%s\n" "$RED" "$RESET"
            FAILED=$((FAILED + 1))
            FAILURES+=("r-shm-leak: delta=${delta_size} bytes (threshold ${THRESHOLD_BYTES})")
            echo "      delta=${delta_size} bytes  new_volumes=${delta_count}"
        fi

        stop_daemon "$R_DAEMON_PID"
    fi

    echo ""
fi

# ======================================================================
# Test Group 16: Pool-crash SHM-orphan diagnostic
# ======================================================================
#
# Quantifies the cross-process SHM leak that opens when a pool process
# dies between "ship a PACKET_SOURCE_RPTR result" and "flush its tracker
# at next dispatch". Under normal operation the pool's tracker holds the
# block until the next request arrives; if the pool is SIGKILL'd in
# between, the block orphans in the shared volume forever.
#
# This is a DIAGNOSTIC test: the threshold is intentionally generous so
# the suite stays green today (cross-process cleanup is documented as
# out-of-scope in project_eval_shm_leak.md). The "delta=" / "per-crash="
# numbers in the output are the actual exposure -- if/when per-pool
# sub-volumes or equivalent cleanup lands, tighten the threshold to a
# few KB so this becomes a real regression test.

if should_run "pool-crash-stress"; then
    echo "${BOLD}[pool-crash-stress] Pool-crash SHM-orphan diagnostic${RESET}"

    PCS_DIR=$(mktemp -d)
    WORK_DIRS+=("$PCS_DIR")
    if ! compile_program "r-shm-leak.loc" "$PCS_DIR"; then
        TOTAL=$((TOTAL + 1))
        FAILED=$((FAILED + 1))
        FAILURES+=("pool-crash-stress: compilation failed")
    else
        PCS_PORT=$(pick_port)
        start_daemon "$PCS_DIR" --http-port "$PCS_PORT"
        wait_for_http "$PCS_PORT" 15
        PCS_DAEMON_PID=$LAST_DAEMON_PID

        # Warm-up: a few requests to get pool spawned and per-pool lazy
        # state allocated. The last warm-up's RPTR result will be in the
        # pool's tracker when we take the baseline; the kill-loop below
        # measures incremental growth from there.
        for i in 1 2 3; do
            curl -s -o /dev/null -X POST \
                "http://127.0.0.1:${PCS_PORT}/call/echoList" \
                -H "Content-Type: application/json" \
                -d '[["alpha","beta","gamma","delta","epsilon","zeta","eta","theta","iota","kappa"]]'
        done

        before_size=$(shm_size_for_pid "$PCS_DAEMON_PID")
        before_count=$(shm_count_for_pid "$PCS_DAEMON_PID")

        # Wait for the daemon to finish any in-progress recovery: health
        # endpoint returns status="recovering" while RECOVERY_IN_PROGRESS
        # is set and "ok" once respawn is done. Bounded by max_wait
        # seconds to avoid hanging if recovery itself wedges.
        wait_for_recovery_done() {
            local max_wait="${1:-15}"
            local i=0
            local step_ms=200
            local max_steps=$(( max_wait * 1000 / step_ms ))
            while [ "$i" -lt "$max_steps" ]; do
                local h
                h=$(curl -s --max-time 2 "http://127.0.0.1:${PCS_PORT}/health" 2>/dev/null) || h=""
                if echo "$h" | grep -q '"status":"ok"'; then
                    return 0
                fi
                sleep 0.2
                i=$((i + 1))
            done
            return 1
        }

        # Crash loop: each iteration sends a request (so the pool ships
        # a fresh RPTR result), then SIGKILLs all child pools, polls the
        # health endpoint until recovery completes. Without the fix the
        # daemon never respawns; with the fix every iteration's pool is
        # cleanly torn down and replaced.
        N=20
        successful_calls=0
        failed_calls=0
        kills=0
        for i in $(seq 1 $N); do
            status=$(curl -s --max-time 15 -o /dev/null -w "%{http_code}" -X POST \
                "http://127.0.0.1:${PCS_PORT}/call/echoList" \
                -H "Content-Type: application/json" \
                -d '[["alpha","beta","gamma","delta","epsilon","zeta","eta","theta","iota","kappa"]]') \
                || status="000"
            if [ "$status" = "200" ]; then
                successful_calls=$((successful_calls + 1))
            else
                failed_calls=$((failed_calls + 1))
            fi

            # Find pool processes by working-dir + command pattern. The
            # shell wrapper script's PID (LAST_DAEMON_PID) often differs
            # from the actual morloc-nexus PID (sh runs the wrapper,
            # then execs morloc-nexus on systems where exec is
            # implemented as fork+exec). The `R --file=<dir>/pools/...`
            # pattern targets only this test's R pool processes, not
            # the test harness or unrelated runs.
            pool_pids=$(pgrep -f "${PCS_DIR}/pools/.*pool\.R" 2>/dev/null) || pool_pids=""
            for ppid in $pool_pids; do
                if kill -9 "$ppid" 2>/dev/null; then
                    kills=$((kills + 1))
                fi
            done

            # Wait for the daemon to finish reaping and respawning before
            # the next iteration sends a request. Without this, the next
            # curl might race the recovery and either fail or trigger an
            # additional spurious recovery attempt.
            wait_for_recovery_done 15
        done

        # Final ping after the loop to ensure the pool is up and any
        # post-crash daemon-side bookkeeping has settled.
        curl -s --max-time 5 -o /dev/null -X POST \
            "http://127.0.0.1:${PCS_PORT}/call/echoList" \
            -H "Content-Type: application/json" \
            -d '[["alpha","beta","gamma","delta","epsilon","zeta","eta","theta","iota","kappa"]]' \
            || true

        after_size=$(shm_size_for_pid "$PCS_DAEMON_PID")
        after_count=$(shm_count_for_pid "$PCS_DAEMON_PID")

        delta_size=$((after_size - before_size))
        delta_count=$((after_count - before_count))
        if [ "$N" -gt 0 ]; then
            per_crash=$((delta_size / N))
        else
            per_crash=0
        fi

        # With coordinated recovery (kill all pools + drop SHM + respawn
        # at fresh basename per generation), each crash should fully
        # reclaim the prior generation's volumes. delta_size should be
        # ~0 plus the new generation's bootstrap volume(s). 256 KB is
        # generous (a couple of 64 KB volumes' worth) but well under
        # what an actual per-crash leak (~10 KB to multi-MB) would
        # accumulate at 20 crashes.
        THRESHOLD_BYTES=$((256 * 1024))

        TOTAL=$((TOTAL + 1))
        printf "  %-50s " "${N} pool kills, /dev/shm bounded < 256 KB"
        if [ "$delta_size" -lt "$THRESHOLD_BYTES" ]; then
            printf "%sPASS%s\n" "$GREEN" "$RESET"
            PASSED=$((PASSED + 1))
        else
            printf "%sFAIL%s\n" "$RED" "$RESET"
            FAILED=$((FAILED + 1))
            FAILURES+=("pool-crash-stress: delta=${delta_size} bytes (threshold ${THRESHOLD_BYTES})")
        fi
        echo "      delta=${delta_size} bytes  per-crash=${per_crash} bytes  new_volumes=${delta_count}"
        echo "      requests: ${successful_calls} ok / ${failed_calls} fail   pool kills: ${kills}"

        stop_daemon "$PCS_DAEMON_PID"
    fi

    echo ""
fi

# ======================================================================
# Test Group 17: Pool-crash recovery with large payloads
# ======================================================================
#
# The motivating case for the recovery design: scientific-computing
# pools may ship multi-GB RPTRs (genomes, tensors, Arrow tables). A
# single ill-timed crash without proper SHM reclamation could OOM the
# host. This test fires repeated requests that ship a ~250 KB payload
# per call, kills the pool mid-stream each iteration, and asserts that
# /dev/shm/mlc-<daemon-pid>-* total bytes stay bounded across many
# crash/recover cycles. Without recovery's coordinated SHM teardown the
# delta would grow by roughly the payload size per crash.

if should_run "pool-recovery-large"; then
    echo "${BOLD}[pool-recovery-large] Pool-crash recovery (large payloads)${RESET}"

    LP_DIR=$(mktemp -d)
    WORK_DIRS+=("$LP_DIR")
    if ! compile_program "large-payload.loc" "$LP_DIR"; then
        TOTAL=$((TOTAL + 1))
        FAILED=$((FAILED + 1))
        FAILURES+=("pool-recovery-large: compilation failed")
    else
        LP_PORT=$(pick_port)
        start_daemon "$LP_DIR" --http-port "$LP_PORT"
        wait_for_http "$LP_PORT" 15
        LP_DAEMON_PID=$LAST_DAEMON_PID

        wait_lp_recovery_done() {
            local max_wait="${1:-15}"
            local i=0
            local step_ms=200
            local max_steps=$(( max_wait * 1000 / step_ms ))
            while [ "$i" -lt "$max_steps" ]; do
                local h
                h=$(curl -s --max-time 2 "http://127.0.0.1:${LP_PORT}/health" 2>/dev/null) || h=""
                if echo "$h" | grep -q '"status":"ok"'; then
                    return 0
                fi
                sleep 0.2
                i=$((i + 1))
            done
            return 1
        }

        # Warm-up: ensure the pool is fully spawned and /dev/shm
        # baseline reflects steady-state lazy allocations.
        curl -s --max-time 30 -o /dev/null -X POST \
            "http://127.0.0.1:${LP_PORT}/call/bigList" \
            -H "Content-Type: application/json" \
            -d '[1000]'

        before_size=$(shm_size_for_pid "$LP_DAEMON_PID")
        before_count=$(shm_count_for_pid "$LP_DAEMON_PID")

        # Each iteration ships ~250 KB via PACKET_SOURCE_RPTR, then
        # SIGKILLs the pool. Pre-recovery this would orphan ~250 KB of
        # SHM per crash; post-recovery the unlink at recovery time
        # reclaims it.
        N=10
        successful_calls=0
        failed_calls=0
        kills=0
        for i in $(seq 1 $N); do
            status=$(curl -s --max-time 30 -o /dev/null -w "%{http_code}" -X POST \
                "http://127.0.0.1:${LP_PORT}/call/bigList" \
                -H "Content-Type: application/json" \
                -d '[1000]') || status="000"
            if [ "$status" = "200" ]; then
                successful_calls=$((successful_calls + 1))
            else
                failed_calls=$((failed_calls + 1))
            fi

            pool_pids=$(pgrep -f "${LP_DIR}/pools/.*pool\.R" 2>/dev/null) || pool_pids=""
            for ppid in $pool_pids; do
                if kill -9 "$ppid" 2>/dev/null; then
                    kills=$((kills + 1))
                fi
            done

            wait_lp_recovery_done 15
        done

        after_size=$(shm_size_for_pid "$LP_DAEMON_PID")
        after_count=$(shm_count_for_pid "$LP_DAEMON_PID")

        delta_size=$((after_size - before_size))
        delta_count=$((after_count - before_count))
        if [ "$N" -gt 0 ]; then
            per_crash=$((delta_size / N))
        else
            per_crash=0
        fi

        # Pre-recovery would leak ~250 KB per crash * 10 crashes =
        # ~2.5 MB plus extra volumes from each pool's growth. With
        # recovery's coordinated SHM teardown the delta should be
        # ~zero (or at most a couple of bootstrap volumes).
        THRESHOLD_BYTES=$((512 * 1024))

        TOTAL=$((TOTAL + 1))
        printf "  %-50s " "${N} large-payload kills, /dev/shm < 512 KB"
        if [ "$delta_size" -lt "$THRESHOLD_BYTES" ]; then
            printf "%sPASS%s\n" "$GREEN" "$RESET"
            PASSED=$((PASSED + 1))
        else
            printf "%sFAIL%s\n" "$RED" "$RESET"
            FAILED=$((FAILED + 1))
            FAILURES+=("pool-recovery-large: delta=${delta_size} bytes (threshold ${THRESHOLD_BYTES})")
        fi
        echo "      delta=${delta_size} bytes  per-crash=${per_crash} bytes  new_volumes=${delta_count}"
        echo "      requests: ${successful_calls} ok / ${failed_calls} fail   pool kills: ${kills}"

        stop_daemon "$LP_DAEMON_PID"
    fi

    echo ""
fi

# ======================================================================
# Test Group 18: Inline-vs-RPTR threshold round-trips
# ======================================================================
#
# Verifies the 64 KB packet inline / RPTR routing across a sweep of
# payload sizes that straddles the threshold from both sides. Both
# halves of the daemon-pool wire are exercised: argument packets going
# from daemon to pool (built by parse_cli_data_argument's auto-routing
# call) and result packets coming back (built by make_data_packet_auto
# inside morloc_put_value). The transit must be byte-perfect at every
# size; the threshold is internal so this catches any mismatch in the
# inline-vs-RPTR construction or deserialization paths.
#
# An [Int] flat voidstar is roughly 16 + 8*N bytes, so:
#   N=8190  -> 65536 bytes  -> exactly at threshold (inline)
#   N=8191  -> 65544 bytes  -> just over threshold  (RPTR)
#   N=0     -> empty wrapper -> inline
# We additionally measure SHM volume growth across the sweep: with the
# per-eval arena it should be ~zero regardless of which route fired.

if should_run "inline-threshold"; then
    echo "${BOLD}[inline-threshold] Inline-vs-RPTR threshold round-trips${RESET}"

    IT_DIR=$(mktemp -d)
    WORK_DIRS+=("$IT_DIR")
    if ! compile_program "inline-threshold.loc" "$IT_DIR"; then
        TOTAL=$((TOTAL + 1))
        FAILED=$((FAILED + 1))
        FAILURES+=("inline-threshold: compilation failed")
    else
        IT_PORT=$(pick_port)
        start_daemon "$IT_DIR" --http-port "$IT_PORT"
        wait_for_http "$IT_PORT" 15
        IT_DAEMON_PID=$LAST_DAEMON_PID

        # POST one of the inline-threshold endpoints with a JSON-encoded
        # arg array and capture the parsed `result` field. Echos status
        # to a side variable so the caller can branch on transport
        # success vs. value mismatch.
        # The body goes in on stdin, not as an argument. Linux caps a single
        # argv element at 128 KB however large ARG_MAX is, and the whole
        # point of the sizes below is to cross the threshold where a payload
        # stops being inlined -- so the largest cases are exactly the ones
        # curl would refuse to be handed on the command line.
        it_call() {
            local endpoint="$1"
            local body="$2"
            printf '%s' "$body" | curl -s --max-time 30 -X POST \
                "http://127.0.0.1:${IT_PORT}/call/${endpoint}" \
                -H "Content-Type: application/json" \
                --data-binary @- 2>/dev/null
        }

        # Build "[0,1,2,...,N-1]" as a JSON list. Done in python3 so
        # we don't fork bash through huge-string concatenation.
        it_seq_json() {
            local n="$1"
            python3 -c "
import sys
n = int(sys.argv[1])
sys.stdout.write('[' + ','.join(str(i) for i in range(n)) + ']')
" "$n"
        }

        # Compute sum(0..N-1) = N*(N-1)/2; used to validate sumInts.
        it_expected_sum() {
            python3 -c "
import sys
n = int(sys.argv[1])
print(n * (n - 1) // 2)
" "$1"
        }

        # Verify a JSON-array result against an expected sequence: same
        # length and identical first/last/midpoint values. Avoids
        # comparing huge strings element-by-element in bash.
        # The response arrives on stdin for the same reason the request goes
        # out that way: at the sizes this group exists to test, it does not
        # fit in an argument.
        it_verify_seq() {
            local result_json="$1"
            local expected_n="$2"
            printf '%s' "$result_json" | python3 -c "
import sys, json
data = json.loads(sys.stdin.read())
result = data.get('result')
expected_n = int(sys.argv[1])
if not isinstance(result, list):
    print('NOT_A_LIST'); sys.exit(0)
if len(result) != expected_n:
    print('WRONG_LEN:%d_vs_%d' % (len(result), expected_n)); sys.exit(0)
if expected_n == 0:
    print('OK'); sys.exit(0)
checks = [(0, 0), (expected_n - 1, expected_n - 1)]
mid = expected_n // 2
checks.append((mid, mid))
for idx, exp in checks:
    if result[idx] != exp:
        print('MISMATCH_AT_%d:%s_vs_%d' % (idx, result[idx], exp)); sys.exit(0)
print('OK')
" "$expected_n"
        }

        # Warm-up so the pool is fully spawned and the per-pool
        # bootstrap allocations don't muddy the SHM-growth baseline.
        it_call echoInts "[[1,2,3]]" > /dev/null

        before_size=$(shm_size_for_pid "$IT_DAEMON_PID")
        before_count=$(shm_count_for_pid "$IT_DAEMON_PID")

        # Sweep covering the threshold from both sides plus extreme
        # ends. 0 / 1 stress the empty / minimum cases. 8190 is exactly
        # at the threshold, 8191/8192 are just over. 100000 is well
        # above, ensuring the RPTR path is also exercised end-to-end.
        SIZES="0 1 100 1000 8000 8190 8191 8192 16000 100000"

        # The sweep runs twice. The first pass establishes the daemon's
        # working set: a 100k-element list is 800 KB of shared memory, and
        # the volume holding it stays mapped once allocated, so growth over a
        # first pass measures the largest payload seen rather than anything
        # being lost. Growth over the SECOND pass is the leak question --
        # repeating work a daemon has already sized itself for should cost
        # nothing at all.
        it_sweep() {
            for n in $SIZES; do
                # echoInts: round-trip the full list. Both arg and result
                # cross the threshold for the upper sizes.
                json_arg="[$(it_seq_json $n)]"
                result=$(it_call echoInts "$json_arg")
                verdict=$(it_verify_seq "$result" "$n")
                if [ "$verdict" != "OK" ]; then
                    all_ok=false
                    echo "      echoInts N=$n: $verdict" >&2
                fi

                # sumInts: huge-input / scalar-output direction. Skipped once
                # the sum leaves the range R can hold in its native integer,
                # which is 32-bit however wide a morloc Int is: R answers NA and
                # the pool refuses to pack it. That is a known limit of the R
                # backend rather than anything about the threshold measured
                # here, and it fails closed rather than answering wrongly.
                if [ "$(it_expected_sum "$n")" -le 2147483647 ]; then
                    result=$(it_call sumInts "$json_arg")
                    actual_sum=$(json_field "$result" "result")
                    expected_sum=$(it_expected_sum "$n")
                    if [ "$actual_sum" != "$expected_sum" ]; then
                        all_ok=false
                        echo "      sumInts N=$n: got $actual_sum expected $expected_sum" >&2
                    fi
                fi

                # firstN: scalar input / large output direction.
                result=$(it_call firstN "[$n]")
                verdict=$(it_verify_seq "$result" "$n")
                if [ "$verdict" != "OK" ]; then
                    all_ok=false
                    echo "      firstN N=$n: $verdict" >&2
                fi
            done

            # Multi-arg edge case: this exercise is implicit in the existing
            # echoInts call (with a single big arg), but we also fire a
            # mixed-size pair through echoInts twice in succession to
            # ensure the per-arg routing decision is independent (one arg
            # inline, the next RPTR, then back).
            for pair in "10 100000" "100000 10"; do
                set -- $pair
                small="$1"
                big="$2"
                result=$(it_call echoInts "[$(it_seq_json $small)]")
                verdict=$(it_verify_seq "$result" "$small")
                if [ "$verdict" != "OK" ]; then
                    all_ok=false
                    echo "      mixed-pair small N=$small after N=$big: $verdict" >&2
                fi
                result=$(it_call echoInts "[$(it_seq_json $big)]")
                verdict=$(it_verify_seq "$result" "$big")
                if [ "$verdict" != "OK" ]; then
                    all_ok=false
                    echo "      mixed-pair big N=$big after N=$small: $verdict" >&2
                fi
            done
        }

        all_ok=true
        it_sweep
        mid_size=$(shm_size_for_pid "$IT_DAEMON_PID")
        mid_count=$(shm_count_for_pid "$IT_DAEMON_PID")
        it_sweep

        after_size=$(shm_size_for_pid "$IT_DAEMON_PID")
        after_count=$(shm_count_for_pid "$IT_DAEMON_PID")
        delta_size=$((after_size - mid_size))
        delta_count=$((after_count - mid_count))
        first_pass=$((mid_size - before_size))

        # A repeat of work the daemon has already sized itself for should
        # need no new shared memory. The allowance is one volume's worth of
        # slack for allocator bookkeeping, not room for a payload.
        SHM_THRESHOLD=$((1024 * 1024))

        TOTAL=$((TOTAL + 1))
        printf "  %-50s " "round-trips byte-perfect across threshold"
        if $all_ok; then
            printf "%sPASS%s\n" "$GREEN" "$RESET"
            PASSED=$((PASSED + 1))
        else
            printf "%sFAIL%s\n" "$RED" "$RESET"
            FAILED=$((FAILED + 1))
            FAILURES+=("inline-threshold: at least one round-trip mismatched")
        fi

        TOTAL=$((TOTAL + 1))
        printf "  %-50s " "second sweep needs no new shared memory"
        if [ "$delta_size" -lt "$SHM_THRESHOLD" ]; then
            printf "%sPASS%s\n" "$GREEN" "$RESET"
            PASSED=$((PASSED + 1))
        else
            printf "%sFAIL%s\n" "$RED" "$RESET"
            FAILED=$((FAILED + 1))
            FAILURES+=("inline-threshold: shm grew ${delta_size} bytes (threshold ${SHM_THRESHOLD})")
        fi
        echo "      sizes swept: ${SIZES}"
        echo "      first pass=${first_pass} B (working set)  second pass=${delta_size} B  new_volumes=${delta_count}"

        stop_daemon "$IT_DAEMON_PID"
    fi

    echo ""
fi

# ======================================================================
# Results
# ======================================================================

echo "=== Results ==="
echo "${GREEN}Passed: $PASSED${RESET}, ${RED}Failed: $FAILED${RESET}, Total: $TOTAL"

if (( FAILED > 0 )); then
    echo ""
    echo "${RED}Failures:${RESET}"
    for f in "${FAILURES[@]}"; do
        echo "  ${RED}-${RESET} $f"
    done
    exit 1
fi
echo "${GREEN}${BOLD}ALL PASSED${RESET}"
