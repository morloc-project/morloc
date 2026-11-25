#!/usr/bin/env bash
# Model-check every TLA+ model in this directory.
#
# Each <Module>.cfg must check clean. Each <Module>_<name>.bug.cfg describes
# a broken variant of the protocol and must report a violation, which shows
# the model can see the fault it guards against. Each module's PlusCal
# translation must be up to date.
#
# usage: check.sh [CONFIG...]   (config names without .cfg; default: all)
#
# TLC_JOBS configs are checked at once (default 2), each with 2 TLC workers.
# A config still running after TLC_TIMEOUT seconds (default 600) fails;
# without a `timeout` command on PATH there is no limit.
#
# Needs java (11+) on PATH. Uses TLA2TOOLS_JAR if set, else downloads the
# pinned release into a cache and verifies its checksum.

set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
version="1.7.4"
sha256="936a262061c914694dfd669a543be24573c45d5aa0ff20a8b96b23d01e050e88"
cache="${XDG_CACHE_HOME:-$HOME/.cache}/morloc"
jar="${TLA2TOOLS_JAR:-$cache/tla2tools-$version.jar}"
jobs="${TLC_JOBS:-2}"
limit="${TLC_TIMEOUT:-600}"
limiter=""
if command -v timeout >/dev/null; then
    limiter="timeout $limit"
fi

if ! command -v java >/dev/null; then
    echo "check.sh: java not found on PATH" >&2
    exit 2
fi

if [ ! -f "$jar" ]; then
    mkdir -p "$(dirname "$jar")"
    curl -sSL -o "$jar.part" \
        "https://github.com/tlaplus/tlaplus/releases/download/v$version/tla2tools.jar"
    mv "$jar.part" "$jar"
fi
if [ -z "${TLA2TOOLS_JAR:-}" ]; then
    echo "$sha256  $jar" | sha256sum -c --quiet - || { rm -f "$jar"; exit 2; }
fi

if [ "$#" -gt 0 ]; then
    names=("$@")
else
    names=()
    for cfg in "$here"/*.cfg; do
        names+=("$(basename "$cfg" .cfg)")
    done
fi
for name in "${names[@]}"; do
    [ -f "$here/$name.cfg" ] || { echo "check.sh: no config $name" >&2; exit 2; }
done

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
cp "$here"/*.tla "$here"/*.cfg "$work"/
mkdir "$work/results"

status=0
for tla in "$here"/*.tla; do
    module="$(basename "$tla" .tla)"
    if grep -q -- '--algorithm' "$tla"; then
        (cd "$work" && java -cp "$jar" pcal.trans -nocfg "$module.tla" >/dev/null)
        if ! diff -q "$tla" "$work/$module.tla" >/dev/null; then
            echo "FAIL $module: PlusCal translation is stale; run pcal.trans on it"
            status=1
        fi
    fi
done
cp "$here"/*.tla "$work"/

check_one() {
    local name="$1" module out rc start secs states line
    module="${name%%_*}"
    module="${module%.bug}"
    start=$SECONDS
    rc=0
    out="$(cd "$work" && $limiter java -Xmx1g -XX:+UseParallelGC -cp "$jar" tlc2.TLC \
        -workers 2 -metadir "$work/states-$name" -config "$name.cfg" "$module" 2>&1)" || rc=$?
    rm -rf "$work/states-$name"
    secs=$((SECONDS - start))
    states="$(grep -oE '[0-9,]+ distinct states' <<<"$out" | tail -1 | tr -d ,)"
    states="${states%% *}"
    if [ -n "$limiter" ] && [ "$rc" -eq 124 ]; then
        line="FAIL $name: still running after $limit s (${states:-0} states)"
    else
        case "$name" in
            *.bug)
                if grep -qE "violated|Deadlock reached" <<<"$out"; then
                    line="ok   $name ($secs s, violation found, as expected)"
                else
                    line="FAIL $name: expected a violation
$(tail -5 <<<"$out")"
                fi
                ;;
            *)
                if grep -q "No error has been found" <<<"$out"; then
                    line="ok   $name ($secs s, $states states)"
                else
                    line="FAIL $name
$(grep -E "Error|violated" <<<"$out" | head -5)"
                fi
                ;;
        esac
    fi
    printf '%s\n' "$line" > "$work/results/$name"
    printf '%s\n' "$line"
}
export -f check_one
export work jar limit limiter

printf '%s\0' "${names[@]}" | xargs -0 -n 1 -P "$jobs" bash -c 'check_one "$1"' _
for name in "${names[@]}"; do
    if ! grep -q '^ok ' "$work/results/$name" 2>/dev/null; then
        status=1
    fi
done
exit $status
