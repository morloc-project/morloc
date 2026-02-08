#!/usr/bin/env bash
# Compare two metric snapshots
# Usage: ./compare-metrics.sh <baseline.json> <current.json>

set -euo pipefail

if [ $# -ne 2 ]; then
    echo "Usage: $0 <baseline.json> <current.json>"
    echo ""
    echo "Example:"
    echo "  $0 metrics/baselines/v0.59.0.json metrics/baselines/v0.60.0.json"
    exit 1
fi

BASELINE="$1"
CURRENT="$2"

if [ ! -f "$BASELINE" ]; then
    echo "Error: Baseline file not found: $BASELINE"
    exit 1
fi

if [ ! -f "$CURRENT" ]; then
    echo "Error: Current file not found: $CURRENT"
    exit 1
fi

# Helper function to extract JSON values
get_value() {
    local file="$1"
    local path="$2"

    # Try with jq if available, otherwise grep
    if command -v jq &> /dev/null; then
        jq -r "$path // \"null\"" "$file" 2>/dev/null || echo "null"
    else
        # Fallback to grep (less reliable)
        grep "\"${path//./}\"" "$file" | sed 's/.*: \(.*\),\?/\1/' | tr -d '"' || echo "null"
    fi
}

# Helper function to calculate percentage change
pct_change() {
    local old="$1"
    local new="$2"

    if [ "$old" = "null" ] || [ "$new" = "null" ]; then
        echo "N/A"
        return
    fi

    # Use bc for floating point arithmetic
    local change=$(echo "scale=2; (($new - $old) / $old) * 100" | bc 2>/dev/null || echo "N/A")

    if [ "$change" = "N/A" ]; then
        echo "N/A"
    else
        echo "${change}%"
    fi
}

# Helper to format change with color (if tput available)
format_change() {
    local pct="$1"
    local inverse="${2:-false}"  # If true, negative is good

    if [ "$pct" = "N/A" ]; then
        echo "$pct"
        return
    fi

    local num=$(echo "$pct" | tr -d '%')
    local color=""

    if command -v tput &> /dev/null; then
        local green=$(tput setaf 2)
        local red=$(tput setaf 1)
        local reset=$(tput sgr0)

        if [ "$inverse" = "true" ]; then
            # For things like build time where decrease is good
            if (( $(echo "$num < 0" | bc -l) )); then
                color="$green"
            elif (( $(echo "$num > 0" | bc -l) )); then
                color="$red"
            fi
        else
            # For things like SLOC where increase might be concerning
            if (( $(echo "$num > 10" | bc -l) )); then
                color="$red"
            elif (( $(echo "$num < -10" | bc -l) )); then
                color="$green"
            fi
        fi

        echo "${color}${pct}${reset}"
    else
        echo "$pct"
    fi
}

echo "=================================="
echo "Morloc Metrics Comparison"
echo "=================================="
echo ""
echo "Baseline: $BASELINE"
echo "Current:  $CURRENT"
echo ""

# Extract versions
BASE_VER=$(get_value "$BASELINE" ".version")
CURR_VER=$(get_value "$CURRENT" ".version")
echo "Comparing: $BASE_VER -> $CURR_VER"
echo ""

# ==== Source Lines ====
echo "--- Source Lines (from scc) ---"
if command -v jq &> /dev/null; then
    BASE_LINES=$(jq -r '.scc[] | select(.Name == "Total") | .Code // 0' "$BASELINE" 2>/dev/null || echo "0")
    CURR_LINES=$(jq -r '.scc[] | select(.Name == "Total") | .Code // 0' "$CURRENT" 2>/dev/null || echo "0")
    BASE_COMMENTS=$(jq -r '.scc[] | select(.Name == "Total") | .Comments // 0' "$BASELINE" 2>/dev/null || echo "0")
    CURR_COMMENTS=$(jq -r '.scc[] | select(.Name == "Total") | .Comments // 0' "$CURRENT" 2>/dev/null || echo "0")
else
    BASE_LINES="N/A"
    CURR_LINES="N/A"
    BASE_COMMENTS="N/A"
    CURR_COMMENTS="N/A"
fi

echo "Code Lines:    $BASE_LINES -> $CURR_LINES  ($(format_change "$(pct_change "$BASE_LINES" "$CURR_LINES")"))"
echo "Comment Lines: $BASE_COMMENTS -> $CURR_COMMENTS  ($(format_change "$(pct_change "$BASE_COMMENTS" "$CURR_COMMENTS")" true))"
echo ""

# ==== Modules ====
echo "--- Module Counts ---"
BASE_MODS=$(get_value "$BASELINE" ".modules.library")
CURR_MODS=$(get_value "$CURRENT" ".modules.library")
echo "Library Modules: $BASE_MODS -> $CURR_MODS  ($(pct_change "$BASE_MODS" "$CURR_MODS"))"
echo ""

# ==== Build Time ====
echo "--- Build Performance ---"
BASE_TIME=$(get_value "$BASELINE" ".build_time_seconds")
CURR_TIME=$(get_value "$CURRENT" ".build_time_seconds")
echo "Build Time: ${BASE_TIME}s -> ${CURR_TIME}s  ($(format_change "$(pct_change "$BASE_TIME" "$CURR_TIME")" true))"
echo ""

# ==== Binary Size ====
echo "--- Binary Size ---"
BASE_SIZE=$(get_value "$BASELINE" ".binary_size_mb")
CURR_SIZE=$(get_value "$CURRENT" ".binary_size_mb")
echo "Binary Size: ${BASE_SIZE}MB -> ${CURR_SIZE}MB  ($(format_change "$(pct_change "$BASE_SIZE" "$CURR_SIZE")" true))"
echo ""

# ==== Benchmarks ====
echo "--- Benchmarks ---"
if command -v jq &> /dev/null; then
    # Compare benchmark means
    jq -r '.benchmarks[]? | .name' "$CURRENT" 2>/dev/null | while read -r bench_name; do
        BASE_MEAN=$(jq -r ".benchmarks[]? | select(.name == \"$bench_name\") | .mean // null" "$BASELINE" 2>/dev/null)
        CURR_MEAN=$(jq -r ".benchmarks[]? | select(.name == \"$bench_name\") | .mean // null" "$CURRENT" 2>/dev/null)

        if [ "$BASE_MEAN" != "null" ] && [ "$CURR_MEAN" != "null" ]; then
            echo "$bench_name: ${BASE_MEAN} -> ${CURR_MEAN}  ($(format_change "$(pct_change "$BASE_MEAN" "$CURR_MEAN")" true))"
        else
            echo "$bench_name: New benchmark"
        fi
    done
else
    echo "Install jq for detailed benchmark comparison"
fi
echo ""

# ==== Summary ====
echo "=================================="
echo "Summary"
echo "=================================="
echo ""
echo "Key changes:"

# Highlight significant changes (>10%)
if [ "$BASE_LINES" != "N/A" ] && [ "$CURR_LINES" != "N/A" ]; then
    LINES_PCT=$(pct_change "$BASE_LINES" "$CURR_LINES" | tr -d '%')
    if (( $(echo "${LINES_PCT#-} > 10" | bc -l 2>/dev/null || echo 0) )); then
        echo "  - Code size changed by $(format_change "$(pct_change "$BASE_LINES" "$CURR_LINES")")"
    fi
fi

if [ "$BASE_TIME" != "null" ] && [ "$CURR_TIME" != "null" ]; then
    TIME_PCT=$(pct_change "$BASE_TIME" "$CURR_TIME" | tr -d '%')
    if (( $(echo "${TIME_PCT#-} > 10" | bc -l 2>/dev/null || echo 0) )); then
        echo "  - Build time changed by $(format_change "$(pct_change "$BASE_TIME" "$CURR_TIME")" true)"
    fi
fi

echo ""
echo "For detailed line-by-line comparison, use jq:"
echo "  diff <(jq . $BASELINE) <(jq . $CURRENT)"
