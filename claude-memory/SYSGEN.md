# System Generation

## Overview

The program builder writes generated code to disk and invokes compilers to create the final executable. The nexus binary is pre-compiled during `morloc init`; `morloc make` only writes a JSON manifest and copies the binary.

## Build.hs

Located in `library/Morloc/ProgramBuilder/Build.hs`

**Responsibilities:**
- Write manifest JSON file to disk
- Copy pre-compiled nexus binary to output path
- Write pool files (pool.py, pool.cpp, pool.R)
- Invoke C++ compiler for C++ pools
- Copy Python and R pools to output directory
- Set executable permissions

## Build Process

```
Code Generator output
  → Write <name>.manifest (JSON)
  → Copy morloc-nexus binary to <name>
  → Write pool files for each language
  → Compile C++ pools with g++/clang++
    - Link with libcppmorloc.a and libmorloc.so
  → Copy Python pools (no compilation)
  → Copy R pools (no compilation)
  → Result: nexus binary + manifest + pool files
```

## Output Structure

After `morloc make -o foo script.loc`:

```
./
  foo               (copy of ~/.local/share/morloc/bin/morloc-nexus)
  foo.manifest      (JSON manifest with commands, pools, schemas)
  pool.py           (Python pool script, if needed)
  pool.cpp          (C++ source)
  pool-cpp.out      (C++ executable, if compiled)
  pool.R            (R pool script, if needed)
```

## Compiler Invocation

**C++ Pool:**
```bash
g++ -O2 --std=c++17 -o pool-cpp.out pool.cpp [flags] -I[includes] -L[libdir] -lmorloc -lcppmorloc -lpthread
```

**Python Pool:** No compilation (interpreted)

**R Pool:** No compilation (interpreted)

## Dependencies

**Required:**
- C compiler (gcc or clang) -- for `morloc init` only
- libmorloc.so (built during `morloc init`)

**Optional (if using those languages):**
- C++ compiler (g++ or clang++)
- Python 3.x
- R

## Error Handling

Build.hs handles:
- Missing compilers (reports helpful error)
- Compilation failures (shows compiler output)
- Permission issues

---
*See also: [[CODEGEN.md]], [[RUNTIME.md]], [[ARCHITECTURE.md]]*
