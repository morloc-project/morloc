#pragma once
#include <cstdint>

// Plain recursion, one frame per level, as a native program would write it.
__attribute__((noinline)) inline int64_t depth(int64_t n) {
    if (n == 0) return 0;
    int64_t r = 1 + depth(n - 1);
    asm volatile("" : "+r"(r));
    return r;
}
