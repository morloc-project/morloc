#!/usr/bin/env bash
# run-tests.sh -- install the runtime with runtime/install.sh into scratch
# prefixes and check what it leaves behind: libmorloc, the nexus and the
# header from the same source, each renamed into place, and nothing changed
# when the build fails.
#
# Usage: ./run-tests.sh
# Builds into a cargo target directory of its own, kept under the state root
# between runs, never into the cache `morloc init` builds in.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
SRC="$(cd "$SCRIPT_DIR/../../../../runtime" && pwd -P)"
INSTALL="$SRC/install.sh"

if ! command -v cargo > /dev/null 2>&1; then
    echo "SKIP: cargo not found"
    exit 0
fi

WORK="$(mktemp -d "${TMPDIR:-/tmp}/morloc-runtime-install.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT

STATE="${MORLOC_STATE:-${MORLOC_HOME:-$HOME/.local/share/morloc}}"
TARGET="$STATE/cache/rust-build-install-test"

case "$(uname -s)" in
    Darwin) LIB=libmorloc.dylib ;;
    *) LIB=libmorloc.so ;;
esac

PASSED=0
FAILED=0
FAILURES=()

pass() { PASSED=$((PASSED + 1)); echo "  ok   $1"; }
fail() { FAILED=$((FAILED + 1)); FAILURES+=("$1: $2"); echo "  FAIL $1: $2"; }

inode() { ls -i "$1" | awk '{print $1}'; }

artifacts() { echo "$1/lib/$LIB" "$1/bin/morloc-nexus" "$1/include/morloc.h"; }

echo "runtime-install (target $TARGET)"

# A wrong argument count prints the usage and changes nothing.
t=usage
if out=$(bash "$INSTALL" "$WORK/only-one" 2>&1); then
    fail $t "accepted one argument"
elif [[ "$out" != *usage* ]]; then
    fail $t "no usage line: $out"
elif [[ -e "$WORK/only-one" ]]; then
    fail $t "created the prefix"
else
    pass $t
fi

# A fresh prefix gets libmorloc, the nexus and the source's own header, and
# the nexus accepts the libmorloc beside it.
t=fresh
HOME1="$WORK/home1"
if ! bash "$INSTALL" "$HOME1" "$TARGET" > "$WORK/fresh.log" 2>&1; then
    fail $t "install failed: $(tail -5 "$WORK/fresh.log")"
else
    missing=""
    for f in $(artifacts "$HOME1"); do
        [[ -f "$f" ]] || missing="$missing $f"
    done
    if [[ -n "$missing" ]]; then
        fail $t "missing:$missing"
    elif ! cmp -s "$HOME1/include/morloc.h" "$SRC/include/morloc.h"; then
        fail $t "installed header differs from $SRC/include/morloc.h"
    elif ! env -u LD_LIBRARY_PATH -u DYLD_LIBRARY_PATH "$HOME1/bin/morloc-nexus" --build-id > /dev/null 2>&1; then
        fail $t "the installed nexus refuses the installed libmorloc"
    elif [[ -e "$HOME1/.runtime-staging" ]]; then
        fail $t "staging directory left behind"
    elif [[ "$(cat "$HOME1/lib/morloc.build-id" 2> /dev/null)" != \
            "$(env -u LD_LIBRARY_PATH -u DYLD_LIBRARY_PATH "$HOME1/bin/morloc-nexus" --build-id)" ]]; then
        fail $t "lib/morloc.build-id is not the installed nexus's build id"
    else
        pass $t
    fi
fi

# A second install gives each artifact a new file rather than writing into
# the old one.
t=renamed
if [[ -f "$HOME1/include/morloc.h" ]]; then
    before=""
    for f in $(artifacts "$HOME1"); do before="$before $(inode "$f")"; done
    if ! bash "$INSTALL" "$HOME1" "$TARGET" > "$WORK/again.log" 2>&1; then
        fail $t "reinstall failed: $(tail -5 "$WORK/again.log")"
    else
        same=""
        set -- $before
        for f in $(artifacts "$HOME1"); do
            [[ "$(inode "$f")" == "$1" ]] && same="$same $f"
            shift
        done
        if [[ -n "$same" ]]; then fail $t "overwritten in place:$same"; else pass $t; fi
    fi
else
    fail $t "no fresh install to reinstall over"
fi

# A build that fails after libmorloc is built, at the nexus, leaves an
# existing install exactly as it was.
t=failure-keeps-install
HOME2="$WORK/home2"
mkdir -p "$HOME2/lib" "$HOME2/bin" "$HOME2/include" "$WORK/fakebin"
for f in $(artifacts "$HOME2"); do echo "old $(basename "$f")" > "$f"; done
REAL_CARGO="$(command -v cargo)"
cat > "$WORK/fakebin/cargo" <<EOF_CARGO
#!/bin/sh
case "\$*" in
    *morloc-nexus*) exit 1 ;;
esac
exec "$REAL_CARGO" "\$@"
EOF_CARGO
chmod +x "$WORK/fakebin/cargo"
if out=$(PATH="$WORK/fakebin:$PATH" bash "$INSTALL" "$HOME2" "$TARGET" 2>&1); then
    fail $t "install succeeded with a failing cargo"
else
    changed=""
    for f in $(artifacts "$HOME2"); do
        [[ "$(cat "$f")" == "old $(basename "$f")" ]] || changed="$changed $f"
    done
    if [[ -n "$changed" ]]; then
        fail $t "changed:$changed"
    elif [[ -e "$HOME2/.runtime-staging" ]]; then
        fail $t "staging directory left behind"
    elif [[ "$out" != *"morloc runtime install failed:"*nexus* ]]; then
        fail $t "no error naming the failed step: $out"
    else
        pass $t
    fi
fi

echo
echo "runtime-install: $PASSED passed, $FAILED failed"
for f in ${FAILURES[@]+"${FAILURES[@]}"}; do echo "  $f"; done
[[ $FAILED -eq 0 ]]
