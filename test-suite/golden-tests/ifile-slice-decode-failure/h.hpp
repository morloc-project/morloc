#pragma once
#include <cstdint>

template <class F>
int64_t applyOnce(F f, int64_t x) { return f(x); }
