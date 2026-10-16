#pragma once
#include <cstdint>

// noinline and the empty asm keep the optimizer from inlining the recursion
// into itself or turning `1 + depth(n - 1)` into a loop, so every level is
// one real frame at -O2.
__attribute__((noinline)) int64_t depth(int64_t n) {
    if (n == 0) return 0;
    int64_t r = depth(n - 1);
    asm volatile("" : "+r"(r));
    return 1 + r;
}
