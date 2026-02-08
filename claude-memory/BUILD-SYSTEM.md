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
         Script{nexus}                  Script{pools}
              ↓                               ↓
    scriptCode: AnchoredDirTree          scriptCode: Files
    scriptMake: [SysCommand]             scriptMake: [gcc/g++/copy]
              ↓                               ↓
       Write files + Compile            Write files + Compile
              ↓                               ↓
         nexus (C binary)               pool binaries/scripts
```

## Key Files

- `library/Morloc/ProgramBuilder/Build.hs` - Writes files, runs compilers
- `library/Morloc/CodeGenerator/Generate.hs` - Orchestrates code generation
- `library/Morloc/CodeGenerator/Nexus.hs` - Generates nexus.c
- `library/Morloc/CodeGenerator/Grammars/Translator/Cpp.hs` - Generates pool.cpp
- `library/Morloc/CodeGenerator/Grammars/Translator/Python3.hs` - Generates pool.py
- `library/Morloc/CodeGenerator/Grammars/Translator/R.hs` - Generates pool.R
- `library/Morloc/CodeGenerator/SystemConfig.hs` - `morloc init` setup

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
- `include/` - morloc.h, cppmorloc.hpp, xxhash.h, mlccpptypes/
- `lib/` - libmorloc.so, libpymorloc.so, librmorloc.so
- `opt/` - pymorloc.c, setup.py, Makefile (build artifacts)
- `tmp/` - Temporary compilation files
- `src/morloc/plane/` - Installed modules
- `.build-config.yaml` - SLURM support flag

**Working directory** (after `morloc make -o foo script.loc`):
- `nexus` - C orchestrator executable
- `pool-py` - Python pool script
- `pool` - C++ pool executable
- `pool.cpp` - C++ source (if kept)

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

**Nexus** (`Nexus.hs:117`):
```bash
gcc -o nexus -O -I~/.local/share/morloc/include nexus.c
```

**C++ Pool** (`Cpp.hs:244`):
```bash
g++ -O3 --std=c++17 -o pool pool.cpp [flags] -I[includes]
```

**Python Pool**: No compilation, copied with executable bit

**R Pool**: No compilation, copied with executable bit

See [[BUILD-TODO.md]] for issues and improvements.
