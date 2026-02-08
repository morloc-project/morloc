# Build System Refactor Plan

## Goal

Reduce compilation time and improve clarity by splitting morloc.h into compiled shared library with minimal headers.

## Current Problem

```
nexus.c:      #include "morloc.h" (7541 lines) → parse/compile everything
pool.cpp:     #include "cppmorloc.hpp"
                → #include "morloc.h" (7541 lines) → parse/compile everything

Every build reads and processes 7541 lines of C code.
```

## Proposed Solution

Split into **4 logical modules** compiled into `libmorloc.so`:

```
morloc-memory.c/h    (~1500 lines) - Shared memory, allocators
morloc-serial.c/h    (~3000 lines) - MessagePack serialization, schemas
morloc-socket.c/h    (~2000 lines) - Socket/daemon communication
morloc-util.c/h      (~1000 lines) - Error macros, utilities, XXHash

morloc.h             (~500 lines)  - Umbrella header (includes all 4)
```

**Headers** contain only:
- Type definitions (structs, enums)
- Function declarations
- Macros (error handling must stay in headers)

**Implementation files** contain:
- All function bodies
- Static helper functions

## File Organization

```
data/morloc/
├── morloc.h              # Umbrella: includes all 4 headers
├── morloc-util.h         # Error macros, basic types
├── morloc-util.c         # Utilities, XXHash implementation
├── morloc-memory.h       # Shared memory types, function declarations
├── morloc-memory.c       # shmalloc, shfree, shinit, abs2rel, etc.
├── morloc-serial.h       # Schema types, pack/unpack declarations
├── morloc-serial.c       # MessagePack serialization implementation
├── morloc-socket.h       # Daemon types, socket function declarations
└── morloc-socket.c       # start_daemon, wait_for_client, etc.
```

## Module Boundaries

**morloc-util.h/c**: Foundation layer, no dependencies
- Error handling macros (RAISE, TRY, ERRMSG)
- FREE, WAIT macros
- Basic utilities (hex, delete_directory)
- XXHash implementation

**morloc-memory.h/c**: Depends on morloc-util
- `shm_t`, `relptr_t`, `absptr_t` types
- `shinit`, `shclose`, `shmalloc`, `shfree`
- `abs2rel`, `rel2abs`
- Volume management

**morloc-serial.h/c**: Depends on morloc-util, morloc-memory
- `Schema` type and parsing
- `pack_with_schema`, `unpack_with_schema`
- Type-specific packers/unpackers
- Schema utilities

**morloc-socket.h/c**: Depends on morloc-util, morloc-memory, morloc-serial
- `language_daemon_t` type
- `start_daemon`, `close_daemon`
- `wait_for_client`, `stream_from_client`
- `send_packet_to_foreign_server`
- Protocol functions (ping/pong, call packets)

**morloc.h**: Umbrella that includes all four
```c
#include "morloc-util.h"
#include "morloc-memory.h"
#include "morloc-serial.h"
#include "morloc-socket.h"
```

## Build Process

### During `morloc init`

**Current** (`SystemConfig.hs:88-106`):
```haskell
-- Create dummy x.c that includes morloc.h, compile to libmorloc.so
TIO.writeFile tmpCFile ("#include \"" <> libmorlocPath <> "\"")
callProcess "gcc" ["-O", "-shared", "-o", soPath, "-fPIC", tmpCFile]
```

**New**:
```haskell
-- Write all 4 implementation files to include/
TIO.writeFile (includeDir </> "morloc-util.c") (DF.embededFileText DF.morlocUtilC)
TIO.writeFile (includeDir </> "morloc-memory.c") (DF.embededFileText DF.morlocMemoryC)
TIO.writeFile (includeDir </> "morloc-serial.c") (DF.embededFileText DF.morlocSerialC)
TIO.writeFile (includeDir </> "morloc-socket.c") (DF.embededFileText DF.morlocSocketC)

-- Compile all .c files into libmorloc.so
let cfiles = ["morloc-util.c", "morloc-memory.c", "morloc-serial.c", "morloc-socket.c"]
let cPaths = map (includeDir </>) cfiles
callProcess "gcc" $
  ["-shared", "-fPIC", "-O2", "-o", soPath] ++
  cPaths ++
  morlocOptions  -- SLURM flag if needed
```

### During `morloc make` (nexus)

**Current** (`Nexus.hs:117`):
```haskell
"gcc -o nexus -O -I" <> includeDir <> " nexus.c"
```

**New**:
```haskell
"gcc -o nexus -O -I" <> includeDir <> " -L" <> libDir <> " nexus.c -lmorloc"
```

### During `morloc make` (C++ pool)

**Current** (`Cpp.hs:244`):
```haskell
"g++ -O3 --std=c++17 -o pool pool.cpp " <> hsep flags <> " " <> hsep incs
```

**New**:
```haskell
"g++ -O3 --std=c++17 -o pool pool.cpp " <>
  "-L" <> libDir <> " -lmorloc " <>
  hsep flags <> " " <> hsep incs
```

## Migration Strategy

### Phase 1: Split Files (No Behavior Change)
1. Create 4 new .c files by copying sections from morloc.h
2. Create 4 new .h files with declarations
3. Keep original morloc.h as-is for backward compatibility
4. Test: Everything still works

### Phase 2: Extract Implementations
1. Move function bodies from morloc.h to .c files
2. Keep only declarations in .h files
3. Update morloc.h to include the 4 headers
4. Test: Compilation still works (headers included, not linked yet)

### Phase 3: Link Shared Library
1. Update SystemConfig.hs to compile .c files
2. Update Nexus.hs to link -lmorloc
3. Update Cpp.hs to link -lmorloc
4. Test: Everything works, compile time reduced

### Phase 4: Clean Up
1. Remove old morloc.h backup
2. Update documentation
3. Measure compile time improvement

## Expected Benefits

**Compile Time**:
- Headers: ~500 lines (declarations) vs 7541 lines (full impl)
- **~15x faster parsing per compilation**
- libmorloc.so compiled once during `morloc init`
- Nexus/pool builds only parse headers, link library

**Clarity**:
- Logical organization: memory, serialization, sockets, utilities
- Each module ~1000-2000 lines, easy to navigate
- Clear dependencies: util → memory → serial → socket

**Development**:
- Easy to find code (know which file to look in)
- Can test modules independently
- Can modify one module without recompiling others

**Simplicity**:
- No build system required
- Simple gcc commands (already in SystemConfig.hs)
- Minimal changes to Nexus.hs and Cpp.hs

## Non-Goals

- No CMake, Makefiles, or build system abstractions
- No complex header dependency management
- No dramatic reorganization of code logic
- No breaking changes to morloc.h API

## Risks & Mitigation

**Risk**: Shared library not found at runtime
**Mitigation**: Already solved - libmorloc.so in ~/.local/share/morloc/lib,
system knows to look there (LD_LIBRARY_PATH or rpath)

**Risk**: Circular dependencies between modules
**Mitigation**: Clear dependency order (util → memory → serial → socket),
enforce during split

**Risk**: Macros can't be in .c files
**Mitigation**: Keep all macros in headers (error handling, FREE, WAIT)

**Risk**: Breaking existing code
**Mitigation**: Phase 1 keeps backward compatibility, test after each phase

## Implementation Estimate

- Phase 1 (split files): 2-3 hours
- Phase 2 (extract impl): 3-4 hours
- Phase 3 (link library): 1-2 hours
- Phase 4 (cleanup): 1 hour
- Testing throughout: 2-3 hours

**Total: ~10-15 hours of focused work**

## Success Criteria

1. ✓ nexus.c compilation time reduced by >10x
2. ✓ pool.cpp compilation time reduced by >10x
3. ✓ All tests pass
4. ✓ morloc.h API unchanged (backward compatible)
5. ✓ Code organized into logical modules
6. ✓ No new build system dependencies

See [[BUILD-SYSTEM.md]] for current architecture.
