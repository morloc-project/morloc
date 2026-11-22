#pragma once
#include <cstdint>
#include <string>
#include <vector>

inline std::vector<int> mk(int n) {
    std::vector<int> xs;
    for (int i = 0; i < n; i++) xs.push_back(10 * n + i);
    return xs;
}

inline std::vector<std::string> renderPieces(const std::vector<int>& xs) {
    if (xs.empty()) return {};
    std::vector<std::string> out{std::string()};
    for (int x : xs) out.push_back(std::to_string(x) + ",");
    out.push_back("\n");
    return out;
}

inline std::vector<std::vector<uint8_t>> renderBytePieces(const std::vector<int>& xs) {
    std::vector<std::vector<uint8_t>> out;
    for (const std::string& s : renderPieces(xs)) out.emplace_back(s.begin(), s.end());
    return out;
}

inline std::vector<std::string> manyPieces(const std::vector<int>& xs) {
    std::vector<std::string> out;
    for (int x : xs)
        for (int i = 0; i < 1000; i++) out.push_back(std::to_string(x) + ":" + std::to_string(i) + "\n");
    return out;
}
