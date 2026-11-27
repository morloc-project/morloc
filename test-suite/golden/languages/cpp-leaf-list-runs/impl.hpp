#pragma once
#include <vector>

inline std::vector<bool> flipBools(const std::vector<bool>& xs) {
    std::vector<bool> r(xs.size());
    for (size_t i = 0; i < xs.size(); i++) r[i] = !xs[i];
    return r;
}

inline std::vector<int> bumpInts(const std::vector<int>& xs) {
    std::vector<int> r;
    for (int x : xs) r.push_back(x + 1);
    return r;
}
