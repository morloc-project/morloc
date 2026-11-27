#pragma once
#include <vector>
#include <tuple>
#include <string>
#include <cstdint>

template <class F, class A>
auto mapv(F f, const std::vector<A>& xs) {
    using B = decltype(f(xs[0]));
    std::vector<B> out;
    out.reserve(xs.size());
    for (const auto& x : xs) out.push_back(f(x));
    return out;
}

inline int64_t dbl(int64_t x) { return x + x; }
