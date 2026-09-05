#pragma once

#include <vector>
#include <string>
#include <stdexcept>
#include <cstdint>

// Returns a single-element list holding the string length, or throws
// on empty input so @catch's fallback path is exercised.
// morloc `Int` maps to C++ `int` (root-cpp: `type Cpp => Int = "int"`), so a
// `[Int]` return must be `std::vector<int>`.
static inline std::vector<int> cpp_maybe_list(const std::string& s) {
    if (s.empty()) {
        throw std::runtime_error("cpp_maybe_list: empty input");
    }
    return std::vector<int>{ static_cast<int>(s.size()) };
}
