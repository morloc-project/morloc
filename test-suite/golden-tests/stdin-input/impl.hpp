#pragma once
#include <vector>
#include <string>
#include <cstdint>

// first n non-negative integers: [0, 1, ..., n-1]
inline std::vector<int> mk(int n) {
    std::vector<int> xs;
    for (int i = 0; i < n; ++i) xs.push_back(i);
    return xs;
}

// identity helpers, used to force pool-binding of the reader commands
// (mirrors how mosm's cutCommand is pool-bound via its sourced cutF/cutS)
inline std::vector<int> echo(std::vector<int> xs) { return xs; }
inline std::vector<std::string> echoStr(std::vector<std::string> xs) { return xs; }
inline int64_t identI(int64_t n) { return n; }
