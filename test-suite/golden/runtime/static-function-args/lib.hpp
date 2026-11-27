#pragma once
#include <vector>

template <class F>
std::vector<int> cmap(F f, const std::vector<int>& xs) {
    std::vector<int> out;
    out.reserve(xs.size());
    for (int x : xs) out.push_back(f(x));
    return out;
}

inline int add(int a, int b) { return a + b; }
