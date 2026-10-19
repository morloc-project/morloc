#pragma once
#include <string>
#include <vector>
inline int str_len(const std::string& s) { return static_cast<int>(s.size()); }
inline int list_sum(const std::vector<int>& xs) {
    int s = 0;
    for (int x : xs) s += x;
    return s;
}
