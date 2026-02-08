# Test Suite

## Overview

Test suite in `test-suite/` includes golden tests, unit tests, and property tests.

## Running Tests

```bash
# All tests
stack test

# Specific pattern
stack test --test-arguments="--pattern='native-morloc'"

# Single golden test
cd test-suite/golden-tests/native-morloc-1
make
diff exp.txt obs.txt
make clean
```

## Golden Tests

**Location:** `test-suite/golden-tests/`

**Structure:**
Each test is a directory with:
- `main.loc` - Morloc source code
- `Makefile` - Compiles and runs test
- `exp.txt` - Expected output
- `obs.txt` - Observed output (generated)

**Categories:**

| Pattern | Purpose |
|---------|---------|
| `native-morloc-*` | Pure morloc functions |
| `interop-*` | Cross-language calls |
| `argument-form-*` | Function argument handling |
| `manifold-form-*` | Multiple implementations |
| `serial-form-*` | Serialization tests |
| `functional-data-*` | Higher-order functions |
| `records-*` | Record types |
| `typeclasses-*` | Typeclass system |

**Adding a Golden Test:**

1. Create directory: `test-suite/golden-tests/my-test/`
2. Write `main.loc`
3. Create `Makefile`:
   ```makefile
   include ../common.mk
   ```
4. Create `exp.txt` with expected output
5. Add test to `test-suite/Main.hs`

## Unit Tests

**Location:** `test-suite/UnitTypeTests.hs`

**Purpose:** Test specific type system components
- Type unification
- Type resolution
- Constraint solving

## Property Tests

**Location:** `test-suite/PropertyTests.hs`

**Purpose:** QuickCheck property-based tests
- Parser round-tripping
- Type inference properties
- Invariants

## Test Entry Point

**File:** `test-suite/Main.hs`

Defines test groups and discovers golden tests.

## Common Issues

**"openBinaryFile: does not exist"**
- Run `morloc init -f`

**Missing module errors**
- Run `morloc install internal root root-py root-cpp root-r math`

**Golden test fails**
- Check `diff exp.txt obs.txt` in test directory
- Verify morloc compiler is up to date (`stack build`)
- Check for missing dependencies

---
*See also: [[DEBUGGING.md]], [[ARCHITECTURE.md]]*
