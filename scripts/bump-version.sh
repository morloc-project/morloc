#!/usr/bin/env bash
# Sync version from ChangeLog.md to package.yaml and Cargo.toml files.
# Usage: after adding a new ChangeLog.md entry, run:
#   ./scripts/bump-version.sh
set -euo pipefail

cd "$(dirname "$0")/.."

if [[ -n "${1:-}" ]]; then
  VERSION="$1"
else
  VERSION=$(head -1 ChangeLog.md | awk '{print $1}')
  if [[ -z "$VERSION" ]]; then
    echo "Error: could not parse version from first line of ChangeLog.md" >&2
    echo "Expected format: X.Y.Z [YYYY-MM-DD]" >&2
    exit 1
  fi
fi

# Every crate must equal the compiler's version, so it must be one that both
# Cabal (no tags) and Cargo (exactly three parts, no leading zeros) accept.
part='(0|[1-9][0-9]*)'
if ! printf '%s\n' "$VERSION" | grep -Eq "^$part\.$part\.$part\$"; then
  echo "Error: version '$VERSION' is not MAJOR.MINOR.PATCH" >&2
  exit 1
fi

echo "Syncing version $VERSION ..."

sed "s/^version:          .*/version:          $VERSION/" package.yaml > package.yaml.tmp
mv package.yaml.tmp package.yaml
echo "  updated package.yaml"

# Regenerate morloc.cabal from package.yaml
if command -v hpack >/dev/null 2>&1; then
  hpack
elif stack exec -- hpack --version >/dev/null 2>&1; then
  stack exec -- hpack
else
  # Direct sed fallback: update the version line in morloc.cabal
  sed "s/^version:        .*/version:        $VERSION/" morloc.cabal > morloc.cabal.tmp
  mv morloc.cabal.tmp morloc.cabal
fi
echo "  updated morloc.cabal"

# Every runtime crate and the Rust binding carry the compiler's version; the
# nexus refuses a manifest from another morloc version (ART-5), init refuses a
# runtime source of another version (SHIM-7), and a unit test (spec-shim-7-2)
# checks the files below.
CRATES="morloc-manifest morloc-nexus morloc-runtime morloc-runtime-types rustmorloc"
for f in runtime/morloc-{manifest,nexus,runtime,runtime-types}/Cargo.toml binders/rust/rustmorloc/Cargo.toml; do
  awk -v version="$VERSION" '
    !done && /^version = "/ { print "version = \"" version "\""; done = 1; next }
    { print }
    END { if (!done) { print FILENAME ": no version line" > "/dev/stderr"; exit 1 } }
  ' "$f" > "$f.tmp"
  mv "$f.tmp" "$f"
  echo "  updated $f"
done

# The path crates' own entries in the locks (tables without a source line).
for f in runtime/Cargo.lock binders/rust/rustmorloc/Cargo.lock; do
  awk -v version="$VERSION" -v crates="$CRATES" '
    BEGIN { n = split(crates, list, " "); for (i = 1; i <= n; i++) ours[list[i]] = 1 }
    function flush() {
      if (count > 0) {
        for (i = 1; i <= count; i++) {
          if (path && name in ours && block[i] ~ /^version = "/) block[i] = "version = \"" version "\""
          print block[i]
        }
      }
      count = 0; path = 1; name = ""
    }
    /^\[\[package\]\]$/ { flush() }
    { block[++count] = $0 }
    /^name = "/ { name = $0; sub(/^name = "/, "", name); sub(/"$/, "", name) }
    /^source = / { path = 0 }
    END { flush() }
  ' path=1 "$f" > "$f.tmp"
  mv "$f.tmp" "$f"
  echo "  updated $f"
done

echo "Done. Verify with: git diff"
