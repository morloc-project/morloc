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

// STL algorithms and utilities
#include <algorithm>
#include <tuple>
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

#endif
