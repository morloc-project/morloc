# System Generation

## Overview

The program builder writes generated code to disk and invokes compilers to create the final executable.

## Build.hs

Located in `library/Morloc/ProgramBuilder/Build.hs`

**Responsibilities:**
- Write generated nexus.c to disk
- Write pool files (pool_py_0.py, pool_cpp_1.cpp, pool_r_2.R)
- Invoke C compiler for nexus
- Invoke C++ compiler for C++ pools
- Copy Python and R pools to output directory
- Set executable permissions

## Build Process

```
Code Generator output
  → Write nexus.c
  → Write pool files for each language
  → Compile nexus.c with gcc/clang
    - Link with msgpack library
    - Link with morloc runtime
  → Compile C++ pools with g++/clang++
    - Link with cppmorloc.hpp
  → Copy Python pools (no compilation)
  → Copy R pools (no compilation)
  → Result: executable nexus + pool files
```

## Output Structure

After `morloc make -o foo script.loc`:

```
foo/              (or current directory if -o not specified)
  nexus           (C executable)
  pool_py_0.py    (Python pool)
  pool_cpp_1.cpp  (C++ source, if needed)
  pool_cpp_1      (C++ executable, if compiled)
  pool_r_2.R      (R pool)
```

## Compiler Invocation

**Nexus (C):**
```bash
gcc nexus.c -o nexus -lmsgpackc -I$MORLOC_HOME/include
```

**C++ Pool:**
```bash
g++ pool_cpp_1.cpp -o pool_cpp_1 -I$MORLOC_HOME/include
```

**Python Pool:** No compilation (interpreted)

**R Pool:** No compilation (interpreted)

## Dependencies

**Required:**
- C compiler (gcc or clang)
- msgpack C library

**Optional (if using those languages):**
- C++ compiler (g++ or clang++)
- Python 3.x
- R

## Error Handling

Build.hs handles:
- Missing compilers (reports helpful error)
- Compilation failures (shows compiler output)
- Missing msgpack library
- Permission issues

---
*See also: [[CODEGEN.md]], [[RUNTIME.md]], [[ARCHITECTURE.md]]*
