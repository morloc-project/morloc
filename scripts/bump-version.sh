#!/usr/bin/env bash
# Sync version from ChangeLog.md to package.yaml and Cargo.toml files.
# Usage: after adding a new ChangeLog.md entry, run:
#   ./scripts/bump-version.sh
set -euo pipefail

cd "$(dirname "$0")/.."

if [[ -n "${1:-}" ]]; then
  VERSION="$1"
  if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
    echo "Error: invalid version '$VERSION', expected X.Y.Z" >&2
    exit 1
  fi
else
  VERSION=$(head -1 ChangeLog.md | grep -oP '^\d+\.\d+\.\d+')
  if [[ -z "$VERSION" ]]; then
    echo "Error: could not parse version from first line of ChangeLog.md" >&2
    echo "Expected format: X.Y.Z [YYYY-MM-DD]" >&2
    exit 1
  fi
fi

echo "Syncing version $VERSION ..."

sed -i "s/^version:          .*/version:          $VERSION/" package.yaml
echo "  updated package.yaml"

# Regenerate morloc.cabal from package.yaml
if command -v hpack >/dev/null 2>&1; then
  hpack
elif stack exec -- hpack --version >/dev/null 2>&1; then
  stack exec -- hpack
else
  # Direct sed fallback: update the version line in morloc.cabal
  sed -i "s/^version:        .*/version:        $VERSION/" morloc.cabal
fi
echo "  updated morloc.cabal"

for f in data/rust/morloc-{nexus,manifest,runtime}/Cargo.toml; do
  sed -i "s/^version = \".*\"/version = \"$VERSION\"/" "$f"
  echo "  updated $f"
done

echo "Done. Verify with: git diff"
