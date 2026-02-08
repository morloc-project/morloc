# Morloc Compiler Metrics

This directory contains tools for collecting and tracking code quality metrics for the morloc compiler.

## Purpose

Track quantitative metrics over time to:
- Monitor code growth and complexity
- Detect performance regressions
- Guide refactoring decisions
- Compare versions objectively

## Collected Metrics

### Code Metrics (via `scc`)
- Lines of code (total, by file, by language)
- Comment ratio
- Blank lines
- Cyclomatic complexity estimates

### Build Metrics
- Full build time (clean build)
- Binary size
- Module counts (library, executable, test, bench)

### Performance Metrics (via `tasty-bench`)
- Parser performance
- Type checker performance
- Code generator performance

## Directory Structure

```
metrics/
├── scripts/
│   ├── collect-metrics.sh   # Collect all metrics and save snapshot
│   └── compare-metrics.sh    # Compare two metric snapshots
├── baselines/
│   └── *.json                # Metric snapshots (version baselines)
└── reports/
    └── *.txt                 # Human-readable metric reports
```

## Usage

### Collect Metrics

```bash
# Collect metrics for current version
./metrics/scripts/collect-metrics.sh v0.59.0

# Or use auto-generated timestamp
./metrics/scripts/collect-metrics.sh
```

This creates:
- `baselines/v0.59.0.json` - Machine-readable metrics
- `reports/v0.59.0.txt` - Human-readable summary

### Compare Versions

```bash
./metrics/scripts/compare-metrics.sh \
    metrics/baselines/v0.59.0.json \
    metrics/baselines/v0.60.0.json
```

Shows changes in:
- Code lines
- Build time
- Binary size
- Benchmark performance

Color-coded output highlights significant changes (>10%).

## Requirements

### Required
- `stack` - Haskell build tool (already required for morloc)
- `bc` - Command-line calculator (usually pre-installed)

### Recommended
- `scc` - Fast line counter with complexity metrics (written in Go)
  - Install: `go install github.com/boyter/scc/v3@latest`
  - Or: `brew install scc` (macOS)
  - Or download from: https://github.com/boyter/scc/releases
  - Without it, only basic metrics are collected

- `jq` - JSON processor for detailed comparison
  - Install: `brew install jq` or `apt-get install jq`
  - Without it, comparison tool is less detailed

## Workflow

### On Release

```bash
# Before release, collect baseline
./metrics/scripts/collect-metrics.sh v0.59.0-baseline

# After changes, collect new metrics
./metrics/scripts/collect-metrics.sh v0.59.0-final

# Compare
./metrics/scripts/compare-metrics.sh \
    metrics/baselines/v0.59.0-baseline.json \
    metrics/baselines/v0.59.0-final.json
```

### During Development

Collect metrics periodically (weekly, monthly) to track trends:

```bash
./metrics/scripts/collect-metrics.sh $(date +%Y-%m-%d)
```

Compare against previous week/month to catch drift.

### After Major Refactoring

```bash
# Before refactoring
./metrics/scripts/collect-metrics.sh before-refactor-auth

# After refactoring
./metrics/scripts/collect-metrics.sh after-refactor-auth

# Verify no regressions
./metrics/scripts/compare-metrics.sh \
    metrics/baselines/before-refactor-auth.json \
    metrics/baselines/after-refactor-auth.json
```

## Interpreting Results

### Good Trends
- ✅ Build time stable or decreasing
- ✅ Comment ratio >15%
- ✅ Benchmark performance stable or improving
- ✅ Binary size stable (unless adding features)

### Warning Signs
- ⚠️  Code lines growing >20% per release
- ⚠️  Build time increasing >15%
- ⚠️  Benchmark performance degrading >10%
- ⚠️  Comment ratio decreasing

### When to Act
- 🚨 Any metric changes >25% unexpectedly
- 🚨 Build time >5 minutes
- 🚨 Binary size >100MB
- 🚨 Average module size >500 lines

## Extending Metrics

To add new metrics, edit `collect-metrics.sh`:

```bash
# Add new metric collection
echo "Collecting custom metric..."
CUSTOM_VALUE=$(your-command)
echo "  \"custom_metric\": $CUSTOM_VALUE," >> "$OUTPUT_FILE"
```

Ensure JSON formatting is valid (commas, no trailing comma on last entry).

## Notes

- Metrics are stored as JSON for easy parsing and analysis
- Scripts are designed to work without external dependencies (except scc/jq)
- Git commit hash is automatically captured in each snapshot
- Build metrics require clean build (slower but reproducible)

## See Also

- `../CLAUDE.md` - Full tooling guidelines including when to run metrics
- `../bench/` - Benchmark suite source code
