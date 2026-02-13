# Build System

The build phase writes generated code to disk, compiles what needs compiling, and assembles the final executable program.

## System Initialization

Before any program can be built, the runtime must be initialized with `morloc init`. This one-time step compiles:

1. **libmorloc.so** -- shared C library providing socket communication, shared memory, msgpack serialization, and error handling.
2. **morloc-nexus** -- static C binary (the nexus), linked against libmorloc.
3. **libcppmorloc.a** -- static C++ library with template-based serialization.
4. **pymorloc** -- Python C extension for msgpack serialization.
5. **rmorloc** -- R C extension for msgpack serialization.

These artifacts are installed to `~/.local/share/morloc/`:

```
bin/morloc-nexus        -- pre-compiled nexus binary
include/morloc.h        -- C runtime header
include/cppmorloc.hpp   -- C++ serialization header
lib/libmorloc.so        -- C runtime library
lib/libcppmorloc.a      -- C++ serialization library
lib/libpymorloc.so      -- Python C extension
lib/librmorloc.so       -- R C extension
```

## Program Build Process

When the user runs `morloc make -o foo script.loc`, the build phase:

1. **Writes the manifest**: `foo.manifest` (JSON file).
2. **Copies the nexus binary**: copies `~/.local/share/morloc/bin/morloc-nexus` to `./foo`.
3. **Writes pool source files**: `pool.py`, `pool.cpp`, `pool.R` as needed.
4. **Compiles C++ pools**: invokes the C++ compiler to produce `pool-cpp.out`.
5. **Sets permissions**: marks interpreted pools and the nexus as executable.

## Output Structure

After a successful build:

```
./foo                  -- nexus binary (copy of morloc-nexus)
./foo.manifest         -- JSON manifest
./pool.py              -- Python pool (if Python functions used)
./pool.cpp             -- C++ pool source (if C++ functions used)
./pool-cpp.out         -- compiled C++ pool
./pool.R               -- R pool (if R functions used)
```

## Compilation Commands

### C++ Pools

```
g++ -O2 --std=c++17 -o pool-cpp.out pool.cpp \
  -I~/.local/share/morloc/include \
  -L~/.local/share/morloc/lib \
  -lmorloc -lcppmorloc -lpthread
```

The C++ compiler, optimization level, and standard are configurable. The pool links against both libmorloc (for socket communication) and libcppmorloc (for serialization).

### Python Pools

No compilation. The generated `pool.py` is an executable script that imports `pymorloc` for serialization and the user's source modules for function implementations.

### R Pools

No compilation. The generated `pool.R` is an executable script that loads `rmorloc` via `dyn.load` and sources the user's R files.

## Runtime Directory

At execution time, the nexus creates a temporary directory for socket files:

```
/tmp/morloc.XXXXXX/
  pipe-py             -- Python pool socket
  pipe-cpp            -- C++ pool socket
  pipe-r              -- R pool socket
```

This directory is cleaned up when the program exits.

## Dependencies

| Requirement | When Needed |
|-------------|-------------|
| C compiler (gcc/clang) | `morloc init` only |
| C++ compiler (g++/clang++) | Programs using C++ functions |
| Python 3 | Programs using Python functions |
| R | Programs using R functions |
| libmorloc.so | Always (runtime communication) |

## Error Handling

The build system reports:
- Missing compilers with actionable error messages.
- Compilation failures with the compiler's full output.
- Missing libraries or headers with paths that were searched.
