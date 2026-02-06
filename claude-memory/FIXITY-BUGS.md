# Fixity System Bugs

## Overview

The morloc fixity system has four critical bugs that violate the specification and prevent correct operator precedence handling.

## Bug 1: Position-Dependent Fixity (Critical)

**Specification:** Fixity declarations should be position-independent.

**Reality:** Fixities only affect expressions parsed after them.

**Example:**
```morloc
z = 1 + 2 * 3        -- Parsed with default fixity
infixl 6 +           -- Too late!
infixl 7 *
```

Expected: `z = 1 + (2 * 3)`
Actual: `z = ((1 + 2) * 3)`

**Root Cause:** `pFixity` calls `CMS.modify'` during parsing, only affecting subsequent parsing.

**Fix:** Two-pass parsing (collect fixities first, then parse expressions).

## Bug 2: FixE Never Processed

**Problem:** `FixE fixity` AST nodes are created but never consumed.

**Evidence:** No pattern matches on `FixE` in Link.hs, Merge.hs, Treeify.hs, or Typecheck.hs.

**Impact:** Fixity declarations don't propagate to later compiler stages.

**Fix:** Add `FixE` case in `Link.hs:processExpr` to extract and store fixities.

## Bug 3: No Cross-Module Propagation

**Specification:** Importing an operator should import its fixity.

**Reality:** No mechanism to propagate fixities through imports.

**Example:**
```morloc
-- ops.loc
module ops ((&))
infixl 1 &

-- main.loc
import ops ((&))
-- & should have fixity InfixL 1
-- Actually has default InfixL 9
```

**Why It Partially Works:** `ParserState` is passed between modules during parsing, so fixities accumulate accidentally.

**Problem:** Doesn't work with pre-compiled modules or separate compilation sessions.

**Fix:** Store fixities in module metadata, merge on import.

## Bug 4: No Conflict Detection

**Problem:** `Map.insert` silently overwrites previous fixities.

**Example:**
```morloc
infixl 5 +
infixl 6 +  -- Should error! Silently overwrites instead
```

**Impact:** Confusing behavior, typos go undetected.

**Fix:** Check for conflicts before inserting:
```haskell
case Map.lookup opName table of
  Just old | old /= new -> error "Conflicting fixity"
  _ -> insert
```

## Summary Table

| Bug | Severity | Fix Complexity |
|-----|----------|----------------|
| Position-dependent fixity | Critical | Medium (two-pass) |
| FixE never processed | High | Low (add Link case) |
| No cross-module propagation | High | Medium (metadata) |
| No conflict detection | Medium | Low (check insert) |

## Two-Pass Solution

**Phase 1:** Collect all fixity declarations before parsing
**Phase 2:** Parse expressions with complete fixity table

See solution details in plan transcript or ask for implementation guidance.

---
*See also: [[PARSING-ALGORITHMS.md]], [[FRONTEND.md]], [[INFIX-OPERATORS.md]]*
