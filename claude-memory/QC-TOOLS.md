# QC Tools

## Installation

```bash
stack install hlint fourmolu weeder
# Stan: Download from https://github.com/kowainik/stan/releases
# scc: go install github.com/boyter/scc/v3@latest
```

## Fourmolu (Formatting)

**Configuration:** `fourmolu.yaml` (2-space, 100-char limit)

```bash
# Format specific files
fourmolu -i library/Morloc/TypeEval.hs

# Format directories
fourmolu -i library/ executable/ test-suite/

# Check without modifying
fourmolu --mode check library/

# All Haskell files
fourmolu -i $(find library executable test-suite -name "*.hs")
```

**When:** Before committing, before PRs

**Not:** Full codebase unless dedicated formatting PR

## HLint (Linting)

**Configuration:** `.hlint.yaml`

```bash
# Lint files
hlint library/Morloc/TypeEval.hs

# Lint directories
hlint library/ executable/ test-suite/

# HTML report
hlint library/ --report=hlint-report.html

# Auto-apply (caution!)
hlint library/ --refactor --refactor-options="-i"
```

**Results:**
- Error: Fix before committing
- Warning: Evaluate case-by-case
- Suggestion: Optional

**When:** After writing new functions, before committing, weekly on full codebase

## Stan (Static Analysis)

```bash
stan  # Generates HTML report
stan --report-format json > stan-report.json
stan --severity=error  # Errors only
```

**Catches:**
- Partial functions, incomplete patterns
- Space leaks, performance issues
- Anti-patterns, code smells
- Dead code, unused bindings

**When:** Before releases, after large refactorings, monthly

**Not:** Small fixes, every commit (too slow)

## Weeder (Dead Code)

**Configuration:** `weeder.yaml`

```bash
stack build
weeder

# With .hie files
stack clean
stack build --ghc-options="-fwrite-ide-info"
weeder
```

Reports unused exports. Use judgment - some exports are intentional API.

**When:** Before releases, after removing features, quarterly

**Not:** During active development (false positives)

## Benchmarks (tasty-bench)

**Location:** `bench/Bench.hs`, `bench/test-data/`

```bash
# All benchmarks
stack bench

# CSV output
stack bench --benchmark-arguments '--csv bench-results.csv'

# Specific groups
stack bench --benchmark-arguments '-p "Parser"'

# Sample size
stack bench --benchmark-arguments '--stdev 5'
```

**When:** Before releases, after optimizations, after refactorings, investigating slowness

## Metrics Collection

**Location:** `metrics/scripts/`

**Collect:**
```bash
./metrics/scripts/collect-metrics.sh v0.59.0
./metrics/scripts/collect-metrics.sh $(date +%Y-%m)
```

**Compare:**
```bash
./metrics/scripts/compare-metrics.sh \
    metrics/baselines/v0.59.0.json \
    metrics/baselines/v0.60.0.json
```

**What:** LOC, module counts, build time, binary size, benchmarks, complexity

**When:** Before releases, after refactorings, monthly snapshots

**Targets (warning signs):**
- Code growth >20%/release
- Build time >5 minutes
- Binary >100MB
- Comments <15%

---
*See also: [[QC.md]]*
