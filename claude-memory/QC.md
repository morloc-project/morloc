# Code Quality

## Claude Instructions

- **DO run hlint** on modified files
- **DO mention** important linting suggestions
- **DON'T run** Stan/Weeder unless requested (slow)
- **DON'T collect metrics** unless performance work or release
- **DO suggest** benchmarks if changes affect performance

## Quick Reference

| Tool | When | Command |
|------|------|---------|
| **Fourmolu** | Before commit | `fourmolu -i file.hs` |
| **HLint** | Before commit | `hlint library/` |
| **Stan** | Before release | `stan` |
| **Weeder** | Before release | `weeder` |
| **Benchmarks** | After optimizations | `stack bench` |
| **Metrics** | Before release | `./metrics/scripts/collect-metrics.sh v0.XX.0` |

See [[QC-TOOLS.md]] for detailed tool documentation.

## Workflows

**Daily Development:**
```bash
fourmolu -i library/Morloc/MyModule.hs
hlint library/Morloc/MyModule.hs
stack test
```

**Before Committing:**
```bash
fourmolu -i $(git diff --name-only --cached | grep '\.hs$')
hlint $(git diff --name-only --cached | grep '\.hs$')
stack test
```

**Weekly Check:**
```bash
hlint library/ executable/ --report=hlint-weekly.html
```

**Pre-Release:**
```bash
# Format
fourmolu -i library/ executable/ test-suite/ bench/

# Lint
hlint library/ executable/ --report=hlint-release.html

# Static analysis
stan

# Dead code
stack build --ghc-options="-fwrite-ide-info"
weeder

# Metrics
./metrics/scripts/collect-metrics.sh v0.XX.0

# Benchmarks
stack bench --benchmark-arguments '--csv bench-v0.XX.0.csv'

# Compare
./metrics/scripts/compare-metrics.sh \
    metrics/baselines/v0.59.0.json \
    metrics/baselines/v0.XX.0.json

# Tests
stack test
```

**After Refactoring:**
```bash
# Before
./metrics/scripts/collect-metrics.sh before-refactor

# After
./metrics/scripts/collect-metrics.sh after-refactor

# Compare
./metrics/scripts/compare-metrics.sh \
    metrics/baselines/before-refactor.json \
    metrics/baselines/after-refactor.json

# Performance check
stack bench

# Issues check
hlint library/
stan
```

---
*See also: [[QC-TOOLS.md]]*
