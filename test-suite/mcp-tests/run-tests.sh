#!/usr/bin/env bash
# run-tests.sh - MCP (Model Context Protocol) server test suite for morloc.
#
# Tests `morloc-nexus mcp <target>`: the JSON-RPC-over-stdio server that
# exposes a compiled program's exported functions as MCP tools. Focus is on
# how docstring / type features map to the MCP tool surface (tools/list) and
# how MCP named arguments invert to the positional call (tools/call), plus the
# protocol handshake, error handling, tool-surface exclusions, and the
# fd-discipline guarantee (stray pool stdout must not corrupt the stream).
#
# Requires a rebuilt nexus: run `morloc init -f` after changing the MCP code.
#
# Usage: ./run-tests.sh [group...]
#   No args runs all groups. Filter by partial name:
#   ./run-tests.sh shape call error protocol fd exclude

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLIENT="$SCRIPT_DIR/mcp_client.py"

PASSED=0
FAILED=0
SKIPPED=0
TOTAL=0
FAILURES=()
WORK_DIRS=()

if [[ -t 1 ]]; then
    GREEN=$'\033[32m' RED=$'\033[31m' YELLOW=$'\033[33m' BOLD=$'\033[1m' RESET=$'\033[0m'
else
    GREEN='' RED='' YELLOW='' BOLD='' RESET=''
fi

cleanup() {
    for d in "${WORK_DIRS[@]}"; do rm -rf "$d"; done
    rm -f /tmp/mcp-stderr-*.log 2>/dev/null || true
}
trap cleanup EXIT

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
skip() { printf "  %-58s %sSKIP%s\n" "$1" "$YELLOW" "$RESET"; SKIPPED=$((SKIPPED+1)); }

assert_eq() { # label expected actual
    if [[ "$3" == "$2" ]]; then pass "$1"; else fail "$1" "$2" "$3"; fi
}
assert_contains() { # label needle haystack
    if grep -qF -- "$2" <<<"$3"; then pass "$1"; else fail "$1" "contains: $2" "$(head -c 200 <<<"$3")"; fi
}
assert_not_contains() { # label needle haystack
    if grep -qF -- "$2" <<<"$3"; then fail "$1" "absent: $2" "$(head -c 200 <<<"$3")"; else pass "$1"; fi
}

# ---------------------------------------------------------------------------
# Client wrappers
# ---------------------------------------------------------------------------

mcp_list() { python3 "$CLIENT" list "$1"; }                 # <target>
mcp_call() { python3 "$CLIENT" call "$1" "$2" "${3:-}"; }   # <target> <tool> [args]
jget()     { python3 "$CLIENT" jget "$2" <<<"$1"; }         # <json> <expr>
# raw messages piped on stdin: raw <target> [--expect-exit]
mcp_raw()  { python3 "$CLIENT" raw "$@"; }

compile_program() { # <loc> <work_dir>  -> 0 ok, 1 fail
    local loc="$1" wd="$2" name
    name="$(basename "$loc" .loc)"
    cp "$SCRIPT_DIR/$loc" "$wd/"
    cp "$SCRIPT_DIR"/*.py "$wd/" 2>/dev/null || true
    if ! (cd "$wd" && morloc make -o nexus "$loc" >/dev/null 2>"$wd/build-${name}.err"); then
        return 1
    fi
    return 0
}

SELECTED=("$@")
should_run() {
    [[ ${#SELECTED[@]} -eq 0 ]] && return 0
    for s in "${SELECTED[@]}"; do [[ "$1" == *"$s"* ]] && return 0; done
    return 1
}

echo "=== Morloc MCP Test Suite ==="
echo ""

# ---------------------------------------------------------------------------
# Compile the shared shapes program up front (most groups use it).
# ---------------------------------------------------------------------------

SHAPES_DIR=$(mktemp -d); WORK_DIRS+=("$SHAPES_DIR")
echo "Compiling shapes.loc ..."
if ! compile_program "shapes.loc" "$SHAPES_DIR"; then
    echo "${RED}FATAL: shapes.loc failed to compile${RESET}"
    cat "$SHAPES_DIR"/build-*.err 2>/dev/null
    exit 1
fi
SHAPES="$SHAPES_DIR/nexus"
echo "Done."
echo ""

# ===========================================================================
# Group: shape -- docstring/type features -> tools/list
# ===========================================================================

if should_run "shape"; then
    echo "${BOLD}[shape] docstring/type -> MCP tool shape${RESET}"
    TOOLS="$(mcp_list "$SHAPES")"

    # -- tool set: name override + which commands are servable ----------------
    assert_eq "all 11 exports are servable tools" "11" "$(jget "$TOOLS" "len(tools)")"
    assert_eq "greet renamed to 'hello' (name:)" "hello" "$(jget "$TOOLS" "'hello' if 'hello' in names else names")"
    assert_not_contains "original name 'greet' absent" "\"greet\"" "$TOOLS"
    assert_contains "pure command 'pureAnswer' present" "pureAnswer" "$TOOLS"

    # -- description: command docstring -> tool description -------------------
    assert_eq "command doc -> description" "Add one to an integer" "$(jget "$TOOLS" "t('addOne')['description']")"

    # -- positionals -> reserved indexed keys ---------------------------------
    assert_eq "positional -> indexed key '_1'" "_1" "$(jget "$TOOLS" "list(t('addOne')['inputSchema']['properties'].keys())[0]")"
    assert_eq "variadic positional -> indexed key '_1'" "_1" "$(jget "$TOOLS" "list(t('sumMany')['inputSchema']['properties'].keys())[0]")"

    # -- scalar types --------------------------------------------------------
    assert_eq "Int arg -> integer" "integer" "$(jget "$TOOLS" "t('addOne')['inputSchema']['properties']['_1']['type']")"
    assert_eq "Str arg -> string" "string" "$(jget "$TOOLS" "t('hello')['inputSchema']['properties']['_1']['type']")"

    # -- required vs optional ------------------------------------------------
    assert_contains "required positional is required" "_1" "$(jget "$TOOLS" "t('addOne')['inputSchema']['required']")"
    assert_eq "?Int not required" "[]" "$(jget "$TOOLS" "t('maybeDouble')['inputSchema']['required']")"
    assert_contains "?Int is nullable" "null" "$(jget "$TOOLS" "t('maybeDouble')['inputSchema']['properties']['_1']['type']")"

    # -- many: true -> array -------------------------------------------------
    assert_eq "variadic -> array" "array" "$(jget "$TOOLS" "t('sumMany')['inputSchema']['properties']['_1']['type']")"
    assert_eq "variadic items -> integer" "integer" "$(jget "$TOOLS" "t('sumMany')['inputSchema']['properties']['_1']['items']['type']")"

    # -- top-level option (arg: + default:) -> non-required property ----------
    assert_eq "positional+option required = [_1]" '["_1"]' "$(jget "$TOOLS" "t('scale')['inputSchema']['required']")"
    assert_contains "top-level option 'factor' present" "factor" "$(jget "$TOOLS" "sorted(t('scale')['inputSchema']['properties'].keys())")"
    assert_contains "option default in description" "default: 2" "$(jget "$TOOLS" "t('scale')['inputSchema']['properties']['factor'].get('description','')")"

    # -- unrolled record -> flat per-field properties ------------------------
    assert_eq "unrolled record -> field count" "3" "$(jget "$TOOLS" "len(t('configFlat')['inputSchema']['properties'])")"
    assert_contains "unrolled field 'tmpDir' present" "tmpDir" "$(jget "$TOOLS" "sorted(t('configFlat')['inputSchema']['properties'].keys())")"
    assert_eq "unrolled Str field -> string" "string" "$(jget "$TOOLS" "t('configFlat')['inputSchema']['properties']['tmpDir']['type']")"
    assert_eq "unrolled Bool field -> boolean (flag)" "boolean" "$(jget "$TOOLS" "t('configFlat')['inputSchema']['properties']['removeCaches']['type']")"
    assert_contains "unrolled default surfaced in description" "default" "$(jget "$TOOLS" "t('configFlat')['inputSchema']['properties'].get('numThreads',{}).get('description','')")"

    # -- record arg: -> whole-object property --------------------------------
    assert_eq "group_opt -> single property" "1" "$(jget "$TOOLS" "len(t('configWhole')['inputSchema']['properties'])")"
    assert_eq "group_opt property named for type" "object" "$(jget "$TOOLS" "t('configWhole')['inputSchema']['properties']['configwhole']['type']")"
    assert_contains "group_opt nests its fields" "tmpDir" "$(jget "$TOOLS" "sorted(t('configWhole')['inputSchema']['properties']['configwhole']['properties'].keys())")"

    # -- return types --------------------------------------------------------
    assert_eq "scalar return -> no outputSchema" "MISSING" "$(jget "$TOOLS" "'PRESENT' if 'outputSchema' in t('addOne') else 'MISSING'")"
    assert_eq "record return -> object outputSchema" "object" "$(jget "$TOOLS" "t('mkResult')['outputSchema']['type']")"
fi

# ===========================================================================
# Group: call -- named arguments -> positional dispatch -> result
# ===========================================================================

if should_run "call"; then
    echo "${BOLD}[call] tools/call inversion + result shaping${RESET}"

    r="$(mcp_call "$SHAPES" addOne '{"_1":41}')"
    assert_eq "addOne{n:41} -> 42 (text)"        "42"    "$(jget "$r" "d['content'][0]['text']")"
    assert_eq "addOne result not isError"        "false" "$(jget "$r" "d.get('isError',False)")"

    r="$(mcp_call "$SHAPES" sumMany '{"_1":[1,2,3,4]}')"
    assert_eq "sumMany variadic -> 10"           "10"    "$(jget "$r" "d['content'][0]['text']")"

    r="$(mcp_call "$SHAPES" scale '{"_1":10}')"
    assert_eq "scale{n:10} -> 20 (default factor)" "20"  "$(jget "$r" "d['content'][0]['text']")"
    r="$(mcp_call "$SHAPES" scale '{"_1":10,"factor":5}')"
    assert_eq "scale{n:10,factor:5} -> 50"       "50"    "$(jget "$r" "d['content'][0]['text']")"

    r="$(mcp_call "$SHAPES" maybeDouble '{"_1":21}')"
    assert_eq "maybeDouble{21} -> 42"            "42"    "$(jget "$r" "d['content'][0]['text']")"
    r="$(mcp_call "$SHAPES" maybeDouble '{}')"
    assert_eq "maybeDouble{} omitted -> null"    "null"  "$(jget "$r" "d['content'][0]['text']")"

    r="$(mcp_call "$SHAPES" hello '{"_1":"World"}')"
    assert_eq "hello{World} -> greeting"         '"Hello, World!"' "$(jget "$r" "d['content'][0]['text']")"

    # unrolled record: omitted fields fall back to typed defaults
    r="$(mcp_call "$SHAPES" configFlat '{}')"
    assert_eq "configFlat{} -> all defaults"     '"/tmp:1:False"' "$(jget "$r" "d['content'][0]['text']")"
    r="$(mcp_call "$SHAPES" configFlat '{"tmpDir":"/x","numThreads":9,"removeCaches":true}')"
    assert_eq "configFlat overrides -> assembled" '"/x:9:True"'   "$(jget "$r" "d['content'][0]['text']")"

    # group_opt: whole object passed under the type-named property
    r="$(mcp_call "$SHAPES" configWhole '{"configwhole":{"tmpDir":"/y","numThreads":3,"removeCaches":false}}')"
    assert_eq "configWhole whole-object -> string" '"/y:3:False"' "$(jget "$r" "d['content'][0]['text']")"
    r="$(mcp_call "$SHAPES" configWhole '{}')"
    assert_eq "configWhole{} -> defaults"        '"/tmp:1:False"' "$(jget "$r" "d['content'][0]['text']")"

    # record return -> structuredContent mirrors the value
    r="$(mcp_call "$SHAPES" mkResult '{"_1":7}')"
    assert_eq "mkResult structuredContent.value" "7"    "$(jget "$r" "d['structuredContent']['value']")"
    assert_eq "mkResult structuredContent.status" "ok"  "$(jget "$r" "d['structuredContent']['status']")"

    # pure command (no pool)
    r="$(mcp_call "$SHAPES" pureAnswer '{}')"
    assert_eq "pure command -> 42"               "42"    "$(jget "$r" "d['content'][0]['text']")"
fi

# ===========================================================================
# Group: error -- protocol errors vs isError results
# ===========================================================================

if should_run "error"; then
    echo "${BOLD}[error] error handling${RESET}"

    # @throw -> a normal result with isError:true (NOT a protocol error)
    r="$(mcp_call "$SHAPES" mustFail '{"_1":1}')"
    assert_eq "@throw -> isError result"         "true"  "$(jget "$r" "d.get('isError',False)")"
    assert_contains "@throw message surfaced"    "boom"  "$(jget "$r" "d['content'][0]['text']")"

    # unknown tool -> JSON-RPC invalid-params error
    r="$(mcp_call "$SHAPES" noSuchTool '{}')"
    assert_eq "unknown tool -> -32602"           "-32602" "$(jget "$r" "d['error']['code']")"

    # unknown argument -> invalid-params
    r="$(mcp_call "$SHAPES" addOne '{"bogus":1}')"
    assert_eq "unknown argument -> -32602"       "-32602" "$(jget "$r" "d['error']['code']")"

    # missing required argument -> invalid-params
    r="$(mcp_call "$SHAPES" addOne '{}')"
    assert_eq "missing required -> -32602"       "-32602" "$(jget "$r" "d['error']['code']")"
fi

# ===========================================================================
# Group: protocol -- handshake, ping, version, id echo, EOF shutdown
# ===========================================================================

if should_run "protocol"; then
    echo "${BOLD}[protocol] JSON-RPC lifecycle${RESET}"

    # tools/list BEFORE init is rejected; after init it succeeds. The
    # initialized notification (no id) yields no response.
    seq="$(mcp_raw "$SHAPES" <<'EOF'
{"jsonrpc":"2.0","id":1,"method":"tools/list"}
{"jsonrpc":"2.0","id":2,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"t","version":"0"}}}
{"jsonrpc":"2.0","method":"notifications/initialized"}
{"jsonrpc":"2.0","id":3,"method":"tools/list"}
EOF
)"
    assert_eq "tools/list before init -> error"  "PRESENT" "$(jget "$seq" "'PRESENT' if 'error' in d['responses'][0] else 'MISSING'")"
    assert_eq "initialize echoes protocolVersion" "2025-06-18" "$(jget "$seq" "d['responses'][1]['result']['protocolVersion']")"
    assert_eq "notification is silent (3 id'd replies)" "3" "$(jget "$seq" "len(d['responses'])")"
    assert_eq "tools/list after init -> ok"      "PRESENT" "$(jget "$seq" "'PRESENT' if 'result' in d['responses'][2] else 'MISSING'")"

    # ping answerable before initialize; id echoed verbatim (int + string)
    seq="$(mcp_raw "$SHAPES" <<'EOF'
{"jsonrpc":"2.0","id":7,"method":"ping"}
{"jsonrpc":"2.0","id":"abc","method":"ping"}
EOF
)"
    assert_eq "ping before init answered"        "7"     "$(jget "$seq" "d['responses'][0]['id']")"
    assert_eq "string id echoed verbatim"        "abc"   "$(jget "$seq" "d['responses'][1]['id']")"

    # version negotiation: an unsupported version -> server advertises its own
    seq="$(mcp_raw "$SHAPES" <<'EOF'
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"1999-01-01","capabilities":{}}}
EOF
)"
    assert_eq "unsupported version -> server's own" "2025-06-18" "$(jget "$seq" "d['responses'][0]['result']['protocolVersion']")"

    # unknown method -> method-not-found
    seq="$(mcp_raw "$SHAPES" <<'EOF'
{"jsonrpc":"2.0","id":5,"method":"does/notExist"}
EOF
)"
    assert_eq "unknown method -> -32601"         "-32601" "$(jget "$seq" "d['responses'][0]['error']['code']")"

    # EOF on stdin ends the session (clean exit)
    seq="$(mcp_raw "$SHAPES" --expect-exit <<'EOF'
{"jsonrpc":"2.0","id":2,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}
{"jsonrpc":"2.0","method":"notifications/initialized"}
EOF
)"
    assert_eq "stdin EOF -> clean exit(0)"       "0"     "$(jget "$seq" "d['exited']")"
fi

# ===========================================================================
# Group: fd -- pool stdout must not corrupt the protocol stream
# ===========================================================================

if should_run "fd"; then
    echo "${BOLD}[fd] fd-discipline (stray pool stdout)${RESET}"

    # noisyAdd's pool prints "STRAY-POOL-STDOUT". A clean, correct result means
    # the stray bytes went to stderr, not the JSON-RPC stream (the client
    # raises ProtocolError on any non-JSON stdout line, so a corrupted stream
    # would fail this call outright rather than mis-parse).
    r="$(mcp_call "$SHAPES" noisyAdd '{"_1":41}')"
    assert_eq "noisy pool -> clean result 42"    "42"    "$(jget "$r" "d['content'][0]['text']")"
    assert_not_contains "no stray bytes in result" "STRAY-POOL-STDOUT" "$r"
fi

# ===========================================================================
# Group: exclude -- unservable commands dropped from tools/list
# ===========================================================================

if should_run "exclude"; then
    echo "${BOLD}[exclude] tool-surface exclusions${RESET}"

    EXCL_DIR=$(mktemp -d); WORK_DIRS+=("$EXCL_DIR")
    if compile_program "excluded.loc" "$EXCL_DIR"; then
        TOOLS="$(mcp_list "$EXCL_DIR/nexus")"
        assert_contains "control 'plainAdd' present"  "plainAdd"  "$TOOLS"
        assert_not_contains "@stdin 'readStdin' excluded" "readStdin" "$TOOLS"
    else
        skip "excluded.loc failed to compile (@stdin group)"
        cat "$EXCL_DIR"/build-*.err 2>/dev/null | head -20
    fi

    # Table exclusion needs pyarrow; skip gracefully if the build fails.
    TBL_DIR=$(mktemp -d); WORK_DIRS+=("$TBL_DIR")
    cp "$SCRIPT_DIR/excluded_table.py" "$TBL_DIR/" 2>/dev/null || true
    if (cd "$TBL_DIR" && cp "$SCRIPT_DIR/excluded_table.loc" . && \
        morloc make -o nexus excluded_table.loc >/dev/null 2>build-table.err); then
        TOOLS="$(mcp_list "$TBL_DIR/nexus")"
        assert_contains "control 'plainAdd' present (table)" "plainAdd" "$TOOLS"
        assert_not_contains "Table-return 'readTable' excluded" "readTable" "$TOOLS"
        assert_not_contains "Table-arg 'sumTable' excluded"     "sumTable"  "$TOOLS"
    else
        skip "excluded_table.loc failed to compile (needs pyarrow)"
    fi
fi

# ===========================================================================
# Summary
# ===========================================================================

echo ""
echo "================================================================"
printf "Total: %d   %sPassed: %d%s   %sFailed: %d%s   %sSkipped: %d%s\n" \
    "$TOTAL" "$GREEN" "$PASSED" "$RESET" "$RED" "$FAILED" "$RESET" "$YELLOW" "$SKIPPED" "$RESET"
if [[ $FAILED -gt 0 ]]; then
    echo ""
    echo "${RED}Failures:${RESET}"
    for f in "${FAILURES[@]}"; do echo "  - $f"; done
    exit 1
fi
echo "${GREEN}All MCP tests passed.${RESET}"
