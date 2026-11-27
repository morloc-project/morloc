// Precompiled header for morloc C++ pools.
// Compiled once during 'morloc init', reused for every pool compilation.

#ifndef MORLOC_PCH_HPP
#define MORLOC_PCH_HPP

// STL containers
#include <vector>
#include <stack>
#include <list>
#include <forward_list>
#include <queue>
#include <deque>
#include <unordered_map>
#include <map>

// STL algorithms and utilities
#include <algorithm>
#include <tuple>
#include <variant>
#include <functional>
#include <limits>
#include <utility>
#include <type_traits>

// Strings and I/O
#include <string>
#include <iostream>
#include <sstream>
#include <fstream>

// C standard library
#include <cstring>
#include <cstdlib>
#include <cstdio>
#include <cstdint>
#include <cmath>
#include <stdexcept>
#include <system_error>

// POSIX headers
#include <sys/stat.h>
#include <sys/mman.h>
#include <unistd.h>
#include <pthread.h>
#include <signal.h>

// Morloc runtime
#include "morloc.h"

// WPOOL-1: the parallel entry, declared ahead of user sources so a
// sourced header (stdlib parallel-cpp) can call it; defined in libcppmorloc.
#define MORLOC_PAR_RUN 1
namespace mlc {
// False when MORLOC_PAR_POOL=0 turns the runtime's helpers off.
bool par_enabled() noexcept;
// Runs body(ctx, i) for each i in [0, nspans) on the calling thread and the
// runtime's helpers, at most `workers` at once; raises the first failure
// again once no helper is on the call.
void par_run(size_t nspans, size_t workers, void (*body)(void*, size_t), void* ctx);
// Tests only: called while a failing span holds its job's lock.
extern void (*par_failure_probe)();
}

#endif
