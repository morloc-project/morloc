# Build System Issues & Improvements

## Critical Issues

### 1. Compiler Hardcoding
**Location**: `Cpp.hs`, `SystemConfig.hs`

**Problem**:
- Hardcoded `gcc` and `g++` with no fallback
- No clang support
- No compiler version detection
- No cross-platform support (Windows, BSD)
- Note: nexus is now compiled during `morloc init` (not per-program), so gcc is only needed at init time

**Impact**: Fails if gcc/g++ not in PATH

**Fix**:
- Add compiler detection in `SystemConfig.hs`
- Try gcc→clang→error for C
- Try g++→clang++→error for C++
- Store detected compilers in Config

### 2. Monolithic morloc.h
**Location**: `data/morloc.h` (7541 lines)

**Problem**:
- Single massive header slows compilation
- No modularization by concern
- Hard to maintain/debug

**Fix**:
- Split into:
  - `morloc-core.h` - Error handling, utilities
  - `morloc-memory.h` - Shared memory, allocators
  - `morloc-serial.h` - MessagePack serialization
  - `morloc-socket.h` - Socket communication
- Update includes in nexus.c and cppmorloc.hpp

### 3. Template Substitution Fragility
**Location**: `Morloc/Data/Doc.hs:63-70`, pool templates

**Problem**:
- String-based split on `"<<<BREAK>>>"`
- No validation of substitution count
- Silent failures if marker count mismatches

**Fix**:
- Add validation: `length replacements == (length splits - 1)`
- Use typed template holes with compile-time checking
- Better error messages on mismatch

### 4. Incomplete SysCommand
**Location**: `Morloc/Namespace.hs:659-665`, `ProgramBuilder/Build.hs:34-37`

**Problem**:
- `SysMove`, `SysInstall`, `SysUnlink` defined but unimplemented
- Limits build flexibility

**Fix**:
- Implement all SysCommand variants
- Or remove unused ones and document why

### 5. No Build Caching
**Problem**:
- Recompiles everything every time
- No incremental builds
- Slow for large projects

**Fix**:
- Hash-based caching:
  - Hash pool source + dependencies
  - Cache compiled pools in `~/.local/share/morloc/cache/`
  - Reuse if hash matches
- Store metadata: `{hash, timestamp, deps, binary_path}`

### 6. Language Binding Asymmetry
**Location**: `data/lang/`

**Problem**:
- Python/R compile to `.so`, C++ is header-only
- Different initialization patterns
- Inconsistent interface styles

**Fix**:
- Unify to header-only pattern (like C++), OR
- Make all compile to `.so` with C ABI
- Document rationale for hybrid approach

## Minor Issues

### 7. Error Propagation Inconsistency
- Nexus uses `ERROR` macro (exits immediately)
- Pools use exception/return patterns
- Inconsistent error message formats

### 8. No Debug Builds
- Always compile with `-O` (nexus) or `-O3` (C++)
- No `-g` flag option for debugging
- Add `--debug` flag to `morloc make`

### 9. Missing Compiler Flags
- No sanitizer support (-fsanitize)
- No position-independent executable (PIE)
- No stack protection flags
- No warning flags (-Wall, -Wextra)

### 10. Socket Path Hardcoding
**Location**: `Config.hs:113`

```haskell
socket = "pipe-" <> pretty (ML.showLangName lang)
```

Could conflict if multiple morloc programs run in same tmpdir.

**Fix**: Include process ID or unique hash in socket names

## Architecture Improvements

### 11. Build System Abstraction
- Create `BuildSystem` module
- Abstract over Make, Ninja, Bazel backends
- Support parallel compilation
- Better dependency tracking

### 12. Compiler Capability Detection
- Detect supported C++ standard (c++14, c++17, c++20)
- Detect available optimizations
- Detect sanitizer support
- Store in build config

### 13. Cross-Compilation Support
- Target triple detection
- Sysroot configuration
- Cross-compiler toolchain specification

### 14. Build Profiles
- `morloc make --profile=debug` (with -g, no optimization)
- `morloc make --profile=release` (with -O3, LTO)
- `morloc make --profile=sanitize` (with ASan, UBSan)

### 15. Better Error Messages
- Show compiler output on failure
- Suggest fixes (install gcc, check PATH)
- Link to documentation

## Performance Improvements

### 16. Parallel Pool Compilation
- Compile pools in parallel (currently sequential)
- Use `async` or similar
- Respect `-j` flag for job count

### 17. Precompiled Headers
- Precompile morloc.h
- Reuse across all nexus/pool compilations
- Significant speedup for large projects

### 18. LTO (Link-Time Optimization)
- Enable for release builds
- Cross-module optimization
- Smaller, faster binaries

## Priority

**High**:
1. Compiler detection (#1)
2. Template validation (#3)
3. Build caching (#5)

**Medium**:
4. Debug builds (#8)
5. Compiler flags (#9)
6. Split morloc.h (#2)

**Low**:
7. Implement SysCommand (#4)
8. Build profiles (#14)
9. Parallel compilation (#16)
10. Everything else

See [[COMPILER-INVOCATION.md]] for compiler detection implementation details.
