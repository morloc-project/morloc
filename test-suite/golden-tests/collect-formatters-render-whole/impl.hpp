#pragma once
#include <vector>
#include <string>

inline std::vector<int> make_ints(int i) {
    return { i * 10 + 3, i * 10 + 1, i * 10 + 2 };
}

// Whole-list render: a size header line, then one line per element. The size
// header is why this needs the WHOLE list (not a per-batch handler).
inline std::string render_whole(std::vector<int> xs) {
    std::string s = "n=" + std::to_string(xs.size()) + "\n";
    for (int x : xs) { s += std::to_string(x); s += "\n"; }
    return s;
}
