#pragma once
#include <cstdint>
#include <string>
#include <vector>

// Batch 0 is empty, 2 renders to a body of 20 MiB plus a line, 1 and 3 to
// one short line each.
inline std::vector<int> mk(int k) {
    if (k == 0) return {};
    return std::vector<int>(1, k);
}

inline std::string render(const std::vector<int>& xs) {
    std::string out;
    for (int k : xs) {
        if (k == 2) out.append(20u << 20, 'x');
        out += "batch " + std::to_string(k) + "\n";
    }
    return out;
}

inline std::string renderBad(const std::vector<int>& xs) {
    return xs.empty() ? std::string() : std::string("a\xff" "b\n");
}

inline std::vector<uint8_t> renderBytes(const std::vector<int>& xs) {
    std::string s = render(xs);
    std::vector<uint8_t> out(s.begin(), s.end());
    out.push_back(0);
    return out;
}

inline std::vector<std::string> badStrsOf(int) {
    return {std::string("ok"), std::string("a\xff" "b")};
}
