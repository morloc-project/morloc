#pragma once
#include <string>
#include <vector>

inline std::string tag(const std::string& s) { return "<" + s + ">"; }

inline int total(const std::vector<int>& xs) {
    int n = 0;
    for (int x : xs) n += x;
    return n;
}
