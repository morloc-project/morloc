#!/usr/bin/env bash
# Collect code metrics for morloc compiler
# Usage: ./collect-metrics.sh [version-label]
#
# Requirements:
#   - scc (lines of code): https://github.com/boyter/scc
#     Install: go install github.com/boyter/scc/v3@latest
#   - stack (Haskell build tool)
#
# This script collects:
#   - Source lines of code (scc)
#   - Module count and structure
#   - Build time
#   - Binary size
#   - Benchmark results (if available)

set -euo pipefail

VERSION="${1:-$(date +%Y%m%d-%H%M%S)}"
METRICS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUTPUT_FILE="$METRICS_DIR/baselines/${VERSION}.json"
REPORT_FILE="$METRICS_DIR/reports/${VERSION}.txt"

echo "=== Collecting metrics for version: $VERSION ==="
echo "Output: $OUTPUT_FILE"
echo ""

# Create output directories
mkdir -p "$METRICS_DIR/baselines"
mkdir -p "$METRICS_DIR/reports"

# Start JSON output
echo "{" > "$OUTPUT_FILE"
echo "  \"version\": \"$VERSION\"," >> "$OUTPUT_FILE"
echo "  \"timestamp\": \"$(date -Iseconds)\"," >> "$OUTPUT_FILE"
echo "  \"git_commit\": \"$(git rev-parse HEAD 2>/dev/null || echo 'unknown')\"," >> "$OUTPUT_FILE"

# ==== Source Lines of Code ====
echo "Collecting source line counts with scc..."
if command -v scc &> /dev/null; then
    scc library/ executable/ test-suite/ bench/ --format json --by-file > "$METRICS_DIR/.scc-temp.json"
    echo "  \"scc\": $(cat "$METRICS_DIR/.scc-temp.json")," >> "$OUTPUT_FILE"
    rm "$METRICS_DIR/.scc-temp.json"

    # Summary for report
    scc library/ executable/ test-suite/ bench/ > "$REPORT_FILE"
else
    echo "  \"scc\": null," >> "$OUTPUT_FILE"
    echo "WARNING: scc not found. Install with: cargo install scc" >&2
    echo "scc not installed" > "$REPORT_FILE"
fi

# ==== Module Statistics ====
echo "Collecting module statistics..."
HASKELL_FILES=$(find library/ -name "*.hs" | wc -l)
EXECUTABLE_FILES=$(find executable/ -name "*.hs" | wc -l)
TEST_FILES=$(find test-suite/ -name "*.hs" | wc -l)
BENCH_FILES=$(find bench/ -name "*.hs" 2>/dev/null | wc -l || echo 0)

echo "  \"modules\": {" >> "$OUTPUT_FILE"
echo "    \"library\": $HASKELL_FILES," >> "$OUTPUT_FILE"
echo "    \"executable\": $EXECUTABLE_FILES," >> "$OUTPUT_FILE"
echo "    \"test\": $TEST_FILES," >> "$OUTPUT_FILE"
echo "    \"bench\": $BENCH_FILES" >> "$OUTPUT_FILE"
echo "  }," >> "$OUTPUT_FILE"

echo "" >> "$REPORT_FILE"
echo "=== Module Counts ===" >> "$REPORT_FILE"
echo "Library: $HASKELL_FILES" >> "$REPORT_FILE"
echo "Executable: $EXECUTABLE_FILES" >> "$REPORT_FILE"
echo "Test: $TEST_FILES" >> "$REPORT_FILE"
echo "Benchmark: $BENCH_FILES" >> "$REPORT_FILE"

# ==== Build Time ====
echo "Measuring build time (clean build)..."
stack clean 2>/dev/null || true
BUILD_START=$(date +%s)
if stack build --force-dirty 2>&1 | tee "$METRICS_DIR/.build-log.txt"; then
    BUILD_END=$(date +%s)
    BUILD_TIME=$((BUILD_END - BUILD_START))
    echo "  \"build_time_seconds\": $BUILD_TIME," >> "$OUTPUT_FILE"

    echo "" >> "$REPORT_FILE"
    echo "=== Build Time ===" >> "$REPORT_FILE"
    echo "$BUILD_TIME seconds" >> "$REPORT_FILE"
else
    echo "  \"build_time_seconds\": null," >> "$OUTPUT_FILE"
    echo "Build failed" >> "$REPORT_FILE"
fi
rm -f "$METRICS_DIR/.build-log.txt"

# ==== Binary Size ====
echo "Measuring binary size..."
INSTALL_ROOT=$(stack path --local-install-root 2>/dev/null || echo "")
if [ -n "$INSTALL_ROOT" ] && [ -f "$INSTALL_ROOT/bin/morloc" ]; then
    BINARY_SIZE=$(stat -f%z "$INSTALL_ROOT/bin/morloc" 2>/dev/null || stat -c%s "$INSTALL_ROOT/bin/morloc" 2>/dev/null || echo 0)
    BINARY_SIZE_MB=$(echo "scale=2; $BINARY_SIZE / 1024 / 1024" | bc)
    echo "  \"binary_size_bytes\": $BINARY_SIZE," >> "$OUTPUT_FILE"
    echo "  \"binary_size_mb\": $BINARY_SIZE_MB," >> "$OUTPUT_FILE"

    echo "" >> "$REPORT_FILE"
    echo "=== Binary Size ===" >> "$REPORT_FILE"
    echo "${BINARY_SIZE_MB} MB" >> "$REPORT_FILE"
else
    echo "  \"binary_size_bytes\": null," >> "$OUTPUT_FILE"
    echo "  \"binary_size_mb\": null," >> "$OUTPUT_FILE"
fi

# ==== Benchmarks ====
echo "Running benchmarks..."
if stack bench --benchmark-arguments '--csv '"$METRICS_DIR/.bench-temp.csv" 2>/dev/null; then
    # Convert CSV to JSON array
    echo "  \"benchmarks\": [" >> "$OUTPUT_FILE"
    tail -n +2 "$METRICS_DIR/.bench-temp.csv" | while IFS=, read -r name mean meanLB meanUB stddev stddevLB stddevUB; do
        echo "    {\"name\": \"$name\", \"mean\": $mean, \"stddev\": $stddev}," >> "$OUTPUT_FILE"
    done
    # Remove trailing comma from last entry
    sed -i '$s/,$//' "$OUTPUT_FILE"
    echo "  ]" >> "$OUTPUT_FILE"
    rm -f "$METRICS_DIR/.bench-temp.csv"

    echo "" >> "$REPORT_FILE"
    echo "=== Benchmarks ===" >> "$REPORT_FILE"
    cat "$METRICS_DIR/.bench-temp.csv" >> "$REPORT_FILE" 2>/dev/null || echo "No benchmark results" >> "$REPORT_FILE"
else
    echo "  \"benchmarks\": []" >> "$OUTPUT_FILE"
    echo "" >> "$REPORT_FILE"
    echo "=== Benchmarks ===" >> "$REPORT_FILE"
    echo "Benchmarks not available or failed" >> "$REPORT_FILE"
fi

# Close JSON
echo "}" >> "$OUTPUT_FILE"

echo ""
echo "=== Metrics collection complete ==="
echo "Results saved to: $OUTPUT_FILE"
echo "Report saved to: $REPORT_FILE"
echo ""
echo "To compare with another version:"
echo "  ./metrics/scripts/compare-metrics.sh baseline.json current.json"
