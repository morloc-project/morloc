#pragma once
#include <string>
#include <vector>
#include <cstdint>
inline std::vector<std::string> bad(int) { return {std::string("ok\n"), std::string("a\xff" "b\n")}; }
inline std::vector<int> mk(int k) { return std::vector<int>(1, k); }
inline std::vector<std::string> cid(const std::vector<std::string>& xs) { return xs; }
inline std::string joinStrs(const std::vector<std::string>& xs) {
    std::string out;
    for (const auto& x : xs) out += x;
    return out;
}
inline std::vector<std::vector<uint8_t>> blobs(int) { return {{0x6f, 0x6b}, {0x61, 0xff, 0x62, 0x0a}}; }
