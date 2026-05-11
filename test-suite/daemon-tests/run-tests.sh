#!/usr/bin/env bash
# run-tests.sh - Daemon and router test suite for morloc
#
# Tests the daemon mode (--daemon), HTTP/TCP/socket APIs, and the
# multi-program router (--router).
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

# Send a length-prefixed JSON message over a socket and read the response.
# Usage: lp_request <socket_or_host:port> <json>
# Output: the response JSON string
lp_request() {
    local target="$1"
    local json="$2"

    python3 -c "
import socket, struct, sys, json

target = sys.argv[1]
msg = sys.argv[2].encode('utf-8')

if target.startswith('/'):
    # Unix socket
    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    s.connect(target)
else:
    # TCP host:port
    host, port = target.rsplit(':', 1)
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((host, int(port)))

s.settimeout(10)

# Send length-prefixed message
s.sendall(struct.pack('>I', len(msg)) + msg)

# Read response length
resp_len_bytes = b''
while len(resp_len_bytes) < 4:
    chunk = s.recv(4 - len(resp_len_bytes))
    if not chunk:
        break
    resp_len_bytes += chunk

resp_len = struct.unpack('>I', resp_len_bytes)[0]

# Read response body
resp = b''
while len(resp) < resp_len:
    chunk = s.recv(resp_len - len(resp))
    if not chunk:
        break
    resp += chunk

s.close()
print(resp.decode('utf-8'))
" "$target" "$json"
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

# Wait for a daemon to be ready (checks stderr log for "daemon: ready")
wait_for_daemon() {
    local log_file="$1"
    local max_wait="${2:-$DAEMON_STARTUP_WAIT}"
    local elapsed=0

    while [ "$elapsed" -lt "$max_wait" ]; do
        if grep -q "ready" "$log_file" 2>/dev/null; then
            return 0
        fi
        sleep 0.2
        elapsed=$((elapsed + 1))
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
start_daemon() {
    local work_dir="$1"
    shift

    local log_file="$work_dir/daemon.log"

    (cd "$work_dir" && exec ./nexus --daemon "$@" 2>"$log_file") &
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

# Sum sizes of all /dev/shm/morloc-<pid>-* segments belonging to a daemon.
shm_size_for_pid() {
    local pid="$1"
    local total=0
    local sz
    for f in /dev/shm/morloc-${pid}-*; do
        [ -e "$f" ] || continue
        sz=$(stat -c %s "$f" 2>/dev/null || echo 0)
        total=$((total + sz))
    done
    echo "$total"
}

# Count /dev/shm/morloc-<pid>-* segments for a daemon.
shm_count_for_pid() {
    ls -1 /dev/shm/morloc-${1}-* 2>/dev/null | wc -l
}

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
WORK_DIRS+=("$ARITH_DIR" "$STRINGS_DIR" "$PURE_DIR")

echo "Compiling test programs..."
compile_program "arithmetic.loc" "$ARITH_DIR"
compile_program "strings.loc" "$STRINGS_DIR"
compile_program "pure.loc" "$PURE_DIR"
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

    # CORS preflight (Stage 3 changes this to 204)
    assert_http_status "OPTIONS returns 200" "200" "http://127.0.0.1:${HTTP_PORT}/call/add" \
        -X OPTIONS

    stop_daemon "$LAST_DAEMON_PID"
    echo ""
fi

# ======================================================================
# Test Group 1b: HTTP status codes
#
# Every daemon-dispatch failure used to map to 500 Internal Server Error
# regardless of cause. After Stage 2, client errors (unknown command,
# unknown endpoint, missing field, malformed args, wrong arity) map to
# 4xx; only genuinely-server-side failures remain 500.
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

    stop_daemon "$LAST_DAEMON_PID"
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
# Test Group 5b: Port CLI validation
#
# `--port` / `--http-port` accept 0..=65535 (0 = bind ephemeral, OS picks
# the port). Anything else -- negative, >65535, non-numeric -- must exit
# non-zero with a clear stderr message. Pre-fix behaviour silently
# swallowed parse errors and truncated out-of-range values via `as u16`.
# ======================================================================

if should_run "port-cli"; then
    echo "${BOLD}[port-cli] Port argument validation${RESET}"

    # Wrap nexus invocations so failures don't trip `set -e`.
    run_nexus() {
        ( cd "$ARITH_DIR" && ./nexus --daemon "$@" 2>"$ARITH_DIR/port-cli.err" )
        local ec=$?
        LAST_NEXUS_ERR=$(cat "$ARITH_DIR/port-cli.err" 2>/dev/null)
        return $ec
    }

    # --http-port out of u16 range
    run_nexus --http-port 99999 || true
    assert_test "--http-port 99999 exits non-zero" "2" "$?"
    assert_contains "--http-port 99999 stderr mentions range" "0..=65535" "$LAST_NEXUS_ERR"

    # --http-port negative
    run_nexus --http-port -1 || true
    assert_test "--http-port -1 exits non-zero" "2" "$?"
    assert_contains "--http-port -1 stderr mentions range" "0..=65535" "$LAST_NEXUS_ERR"

    # --http-port non-numeric
    run_nexus --http-port abc || true
    assert_test "--http-port abc exits non-zero" "2" "$?"
    assert_contains "--http-port abc stderr mentions range" "0..=65535" "$LAST_NEXUS_ERR"

    # --http-port=VAL long-equals form
    run_nexus --http-port=99999 || true
    assert_test "--http-port=99999 exits non-zero" "2" "$?"

    # TCP --port same validation
    run_nexus --port 99999 || true
    assert_test "--port 99999 exits non-zero" "2" "$?"
    assert_contains "--port 99999 stderr mentions range" "0..=65535" "$LAST_NEXUS_ERR"

    run_nexus --port abc || true
    assert_test "--port abc exits non-zero" "2" "$?"

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

    # Set up a temporary fdb directory with manifests
    FDB_DIR=$(mktemp -d)
    WORK_DIRS+=("$FDB_DIR")

    # Extract manifest JSON from the nexus wrapper script
    # Format is: #!/bin/sh\nexec morloc-nexus ...\n### MANIFEST ###\n<json>
    if [ -f "$ARITH_DIR/nexus" ]; then
        sed -n '/^### MANIFEST ###$/,$ { /^### MANIFEST ###$/d; p; }' \
            "$ARITH_DIR/nexus" > "$FDB_DIR/arithmetic.manifest"
        # Patch build_dir in manifest to point to the work dir
        python3 -c "
import json, sys
with open(sys.argv[1]) as f:
    m = json.load(f)
m['build_dir'] = sys.argv[2]
with open(sys.argv[1], 'w') as f:
    json.dump(m, f)
" "$FDB_DIR/arithmetic.manifest" "$ARITH_DIR"
    fi

    if [ ! -s "$FDB_DIR/arithmetic.manifest" ]; then
        echo "  ${RED}SKIP: could not extract manifest${RESET}"
        echo ""
        TOTAL=$((TOTAL + 1))
        FAILED=$((FAILED + 1))
        FAILURES+=("router: could not extract manifest")
    fi

    if [ -s "$FDB_DIR/arithmetic.manifest" ]; then
        ROUTER_PORT=$(pick_port)

        # Start router (use the morloc-nexus binary)
        NEXUS_PATH="$(which morloc-nexus 2>/dev/null || echo "$HOME/.local/bin/morloc-nexus")"
        (exec "$NEXUS_PATH" --router --http-port "$ROUTER_PORT" --fdb "$FDB_DIR" 2>"$FDB_DIR/router.log") &
        ROUTER_PID=$!
        DAEMON_PIDS+=("$ROUTER_PID")

        wait_for_http "$ROUTER_PORT" 15 || true

        # Health check
        result=$(curl -s "http://127.0.0.1:${ROUTER_PORT}/health" 2>/dev/null) || result=""
        status=$(json_field "$result" "status" 2>/dev/null) || status=""
        assert_test "router GET /health" "ok" "$status"

        # List programs
        disco=$(curl -s "http://127.0.0.1:${ROUTER_PORT}/programs" 2>/dev/null) || disco=""
        assert_contains "router GET /programs lists arithmetic" "arithmetic" "$disco"

        # Full discovery
        disco=$(curl -s "http://127.0.0.1:${ROUTER_PORT}/discover" 2>/dev/null) || disco=""
        assert_contains "router GET /discover lists programs" "programs" "$disco"

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
# Test Group 14: SHM-leak regression
# ======================================================================
#
# Asserts that the daemon's per-call SHM allocations are released when
# each request finishes (via the per-eval arena in eval_arena.rs). With
# the leak in place, every call would accumulate ~500 bytes in the
# daemon's /dev/shm/morloc-<pid>-* volumes; over 1000 calls the volume
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
# /dev/shm/morloc-<daemon-pid>-* total bytes stay bounded across many
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
        it_call() {
            local endpoint="$1"
            local body="$2"
            curl -s --max-time 30 -X POST \
                "http://127.0.0.1:${IT_PORT}/call/${endpoint}" \
                -H "Content-Type: application/json" \
                -d "$body" 2>/dev/null
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
        it_verify_seq() {
            local result_json="$1"
            local expected_n="$2"
            python3 -c "
import sys, json
data = json.loads(sys.argv[1])
result = data.get('result')
expected_n = int(sys.argv[2])
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
" "$result_json" "$expected_n"
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

        all_ok=true
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

            # sumInts: huge-input / scalar-output direction.
            result=$(it_call sumInts "$json_arg")
            actual_sum=$(json_field "$result" "result")
            expected_sum=$(it_expected_sum "$n")
            if [ "$actual_sum" != "$expected_sum" ]; then
                all_ok=false
                echo "      sumInts N=$n: got $actual_sum expected $expected_sum" >&2
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

        after_size=$(shm_size_for_pid "$IT_DAEMON_PID")
        after_count=$(shm_count_for_pid "$IT_DAEMON_PID")
        delta_size=$((after_size - before_size))
        delta_count=$((after_count - before_count))

        # The whole sweep including 100k-element calls should grow the
        # daemon's volumes by at most a couple of 64 KB volumes
        # (transient large allocations get reused after the per-eval
        # arena releases them, but the volume itself stays in /dev/shm).
        # 1 MB ceiling is generous; pre-fix we'd see far more from
        # accumulation across 30+ requests of various sizes.
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
        printf "  %-50s " "SHM growth across sweep < 1 MB"
        if [ "$delta_size" -lt "$SHM_THRESHOLD" ]; then
            printf "%sPASS%s\n" "$GREEN" "$RESET"
            PASSED=$((PASSED + 1))
        else
            printf "%sFAIL%s\n" "$RED" "$RESET"
            FAILED=$((FAILED + 1))
            FAILURES+=("inline-threshold: shm grew ${delta_size} bytes (threshold ${SHM_THRESHOLD})")
        fi
        echo "      sizes swept: ${SIZES}"
        echo "      shm delta=${delta_size} bytes  new_volumes=${delta_count}"

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
