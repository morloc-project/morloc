#pragma once

// C++ implementations for the uint8-cross-language golden test.
//
// The morloc type [UInt8] maps to std::vector<uint8_t> here (inherited
// from root-cpp's generic `(List a) = "std::vector<$1>" a`). The
// vector is a packed 1-byte-per-element buffer, identical in storage
// to std::string; both are valid mappings and either could be picked
// without performance cost. This test fixes the choice to vector for
// determinism.

#include <cstdint>
#include <string>
#include <vector>

inline std::vector<uint8_t> cpp_encode(std::string s) {
    return std::vector<uint8_t>(s.begin(), s.end());
}

inline std::string cpp_decode(std::vector<uint8_t> v) {
    return std::string(v.begin(), v.end());
}

inline std::vector<uint8_t> cpp_echo(std::vector<uint8_t> v) {
    return v;
}

inline std::string cpp_native(std::vector<uint8_t> v) {
    return "c++: std::vector<uint8_t>, size=" + std::to_string(v.size());
}
