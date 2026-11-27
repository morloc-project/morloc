#pragma once
#include <cstdint>
#include <string>
#include <vector>

inline int add_batch(int acc, const std::vector<int64_t>& xs) { return acc + (int)xs.size(); }
inline int merge(int a, int b) { return a + b; }
inline std::string show_n(int n) { return "cpp n=" + std::to_string(n) + "\n"; }

inline std::string show_ints(const std::vector<int64_t>& xs) {
    std::string out = "cpp";
    for (auto x : xs) out += " " + std::to_string(x);
    return out + "\n";
}
