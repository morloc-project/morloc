#ifndef __HELPERS_HPP__
#define __HELPERS_HPP__

#include <cstdio>

// Cpp-only source functions used to anchor morloc manifolds to C++ so
// cross-language `<IO> T` returns land at C++-declared receiver proxies.

int cpp_io(int x) {
    std::fprintf(stderr, "cpp:%d\n", x);
    return x + 200;
}

bool cpp_gate(int x) {
    return x > 0;
}

#endif
