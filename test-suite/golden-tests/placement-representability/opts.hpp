#pragma once
#include <cstdint>
#include <functional>
struct Opts { int64_t n; };
inline int64_t nOf(const Opts& o) { return o.n * 3; }
inline int64_t cpp_apply(std::function<int64_t(int64_t)> f, int64_t x) { return f(x); }
inline int64_t cpp_inc(int64_t x) { return x + 1; }
