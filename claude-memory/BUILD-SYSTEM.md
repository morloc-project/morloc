# Build System

## Overview

Morloc generates multi-language executables: a C **nexus** orchestrator and language-specific **pools** (Python, C++, R) that communicate via Unix sockets and msgpack.

## Core Components

**morloc.h** (7541 lines): Universal runtime in C
- Error handling macros (RAISE, TRY, ERRMSG)
- Shared memory management (shmalloc, shfree, shinit)
- MessagePack serialization (pack_with_schema, unpack_with_schema)
- Socket communication (start_daemon, wait_for_client, send_packet_to_foreign_server)
- Protocol layer (call packets, data packets, ping/pong)

**Language Bindings**:
- `pymorloc.c` → `libpymorloc.so` (Python C extension)
- `cppmorloc.hpp` (C++ template header wrapping morloc.h)
- `rmorloc.c` → `librmorloc.so` (R C interface)

**Pool Templates** (`data/pools/`):
- `pool.cpp`, `pool.py`, `pool.R`
- Contain `<<<BREAK>>>` markers for code injection
- Include morloc.h (C++) or language bindings (Python, R)

## Build Pipeline

```
.loc → Parse → Typecheck → Generate → Build → Execute
                              ↓
                    (Script, [Script])
                              ↓
              ┌───────────────┴───────────────┐
              ↓                               ↓
         Script{manifest}              Script{pools}
              ↓                               ↓
    scriptCode: .manifest file          scriptCode: Files
    scriptMake: [cp nexus, chmod]       scriptMake: [g++/copy]
              ↓                               ↓
    Write manifest + copy binary       Write files + Compile
              ↓                               ↓
    nexus binary + .manifest           pool binaries/scripts
```

## Key Files

- `library/Morloc/ProgramBuilder/Build.hs` - Writes files, runs compilers
- `library/Morloc/CodeGenerator/Generate.hs` - Orchestrates code generation
- `library/Morloc/CodeGenerator/Nexus.hs` - Generates JSON manifest
- `library/Morloc/CodeGenerator/Grammars/Translator/Cpp.hs` - Generates pool.cpp
- `library/Morloc/CodeGenerator/Grammars/Translator/Python3.hs` - Generates pool.py
- `library/Morloc/CodeGenerator/Grammars/Translator/R.hs` - Generates pool.R
- `library/Morloc/CodeGenerator/SystemConfig.hs` - `morloc init` setup (compiles nexus + libmorloc)

## Script Data Structure

```haskell
data Script = Script
  { scriptBase :: String              -- "nexus" | "pool"
  , scriptLang :: Lang                -- CLang | CppLang | Python3Lang | RLang
  , scriptCode :: AnchoredDirTree Code  -- File tree to write
  , scriptMake :: [SysCommand]        -- Build commands
  }

data SysCommand
  = SysRun Code      -- Shell command (gcc/g++/etc)
  | SysExe Path      -- Set executable bit
  | SysMove Path Path   -- Unimplemented
  | SysInstall [String] -- Unimplemented
  | SysUnlink Path      -- Unimplemented
```

## Directory Structure

**~/.local/share/morloc/**:
- `bin/` - morloc-nexus (pre-compiled static nexus binary)
- `include/` - morloc.h, cppmorloc.hpp, morloc_pch.hpp, mlccpptypes/
- `lib/` - libmorloc.so, libcppmorloc.a, libpymorloc.so, librmorloc.so
- `opt/` - pymorloc.c, setup.py, Makefile (build artifacts)
- `tmp/` - Temporary compilation files
- `src/morloc/plane/` - Installed modules
- `.build-config.yaml` - SLURM support flag

**Working directory** (after `morloc make -o foo script.loc`):
- `foo` - Copy of morloc-nexus binary
- `foo.manifest` - JSON manifest (commands, pools, schemas)
- `pool.py` - Python pool script
- `pool-cpp.out` - C++ pool executable
- `pool.cpp` - C++ source

**Runtime** (`/tmp/morloc.XXXXXX/`):
- `pipe-py`, `pipe-cpp`, `pipe-r` - Unix domain sockets
- Shared memory volumes in `/dev/shm/`

## Template Substitution

Pool templates use `<<<BREAK>>>` markers:

```cpp
// AUTO include statements start
// <<<BREAK>>>
// AUTO include statements end
```

The `format` function (`Morloc/Data/Doc.hs:63-70`) splits template by breaker, interleaves generated code:

```haskell
format template "// <<<BREAK>>>" [generated0, generated1, ...]
  = text0 + generated0 + text1 + generated1 + ...
```

## Compilation Commands

**Nexus** (compiled once during `morloc init` in `SystemConfig.hs`):
```bash
gcc -O2 -I~/.local/share/morloc/include -o morloc-nexus nexus.c -L<libDir> -Wl,-rpath,<libDir> -lmorloc -lpthread
```

**C++ Pool** (`Cpp.hs`):
```bash
g++ -O2 --std=c++17 -o pool-cpp.out pool.cpp [flags] -I[includes] -L[libdir] -lmorloc -lcppmorloc -lpthread
```

**Python Pool**: No compilation, copied with executable bit

**R Pool**: No compilation, copied with executable bit

See [[BUILD-TODO.md]] for issues and improvements.
