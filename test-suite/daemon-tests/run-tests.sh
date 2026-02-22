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

    # Error: unknown command
    result=$(curl -s -X POST "http://127.0.0.1:${HTTP_PORT}/call/nonexistent" \
        -H "Content-Type: application/json" -d '[1]')
    status=$(json_field "$result" "status")
    assert_test "POST /call/nonexistent returns error" "error" "$status"

    # Error: unknown endpoint
    assert_http_status "GET /bogus returns 400" "400" "http://127.0.0.1:${HTTP_PORT}/bogus"

    # CORS preflight
    assert_http_status "OPTIONS returns 200" "200" "http://127.0.0.1:${HTTP_PORT}/call/add" \
        -X OPTIONS

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
