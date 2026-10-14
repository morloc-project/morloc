#pragma once

// Cost functions for the parallel benchmarks (see parallel/bench/main.loc).

#include <cstdint>
#include <string>
#include <vector>

inline int spin(int n) {
    uint32_t x = 1;
    for (int i = 0; i < n; ++i) x = (x * 1103515245u + 12345u) & 0x7FFFFFFFu;
    return static_cast<int>(x % 1000u);
}

inline bool spinKeep(int n) { return spin(n) % 2 == 0; }

inline std::vector<int> spinList(int n) {
    const int v = spin(n);
    return std::vector<int>(v % 4, v);
}

inline std::string makeStr(int n) { return std::string(n, 'x'); }

inline int strCost(const std::string& s) { return static_cast<int>(s.size() % 1000); }
