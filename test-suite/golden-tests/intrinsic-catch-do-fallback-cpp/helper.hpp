#pragma once

#include <vector>
#include <string>
#include <stdexcept>
#include <cstdint>

// Returns a single-element list holding the string length, or throws
// on empty input so @catch's fallback path is exercised.
static inline std::vector<int64_t> cpp_maybe_list(const std::string& s) {
    if (s.empty()) {
        throw std::runtime_error("cpp_maybe_list: empty input");
    }
    return std::vector<int64_t>{ static_cast<int64_t>(s.size()) };
}
