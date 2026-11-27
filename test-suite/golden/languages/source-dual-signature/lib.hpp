#pragma once
#include <cstdint>
#include <vector>

std::vector<int64_t> double_all(const std::vector<int64_t>& xs) {
    std::vector<int64_t> r;
    for (auto x : xs) r.push_back(2 * x);
    return r;
}
