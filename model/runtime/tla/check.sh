#!/usr/bin/env bash
# Model-check every TLA+ model in this directory.
#
# Each <Module>.cfg must check clean. Each <Module>_<name>.bug.cfg describes
# a broken variant of the protocol and must report a violation, which shows
# the model can see the fault it guards against. Each module's PlusCal
# translation must be up to date.
#
# Needs java (11+) on PATH. Uses TLA2TOOLS_JAR if set, else downloads the
# pinned release into a cache and verifies its checksum.

set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
version="1.7.4"
sha256="936a262061c914694dfd669a543be24573c45d5aa0ff20a8b96b23d01e050e88"
cache="${XDG_CACHE_HOME:-$HOME/.cache}/morloc"
jar="${TLA2TOOLS_JAR:-$cache/tla2tools-$version.jar}"

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

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
cp "$here"/*.tla "$here"/*.cfg "$work"/

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

for cfg in "$here"/*.cfg; do
    name="$(basename "$cfg" .cfg)"
    module="${name%%_*}"
    module="${module%.bug}"
    out="$(cd "$work" && java -Xmx1g -XX:+UseParallelGC -cp "$jar" tlc2.TLC \
        -workers 2 -metadir "$work/states-$name" -config "$name.cfg" "$module" 2>&1 || true)"
    case "$name" in
        *.bug)
            if grep -qE "violated|Deadlock reached" <<<"$out"; then
                echo "ok   $name (violation found, as expected)"
            else
                echo "FAIL $name: expected a violation"
                echo "$out" | tail -5
                status=1
            fi
            ;;
        *)
            if grep -q "No error has been found" <<<"$out"; then
                echo "ok   $name"
            else
                echo "FAIL $name"
                echo "$out" | grep -E "Error|violated" | head -5
                status=1
            fi
            ;;
    esac
done
exit $status
