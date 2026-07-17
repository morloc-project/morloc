#pragma once

#include <string>
#include <stdexcept>

static inline long cpp_len(const std::string& s) {
    return static_cast<long>(s.size());
}

static inline long cpp_may_fail(const std::string& s) {
    if (s.empty()) {
        throw std::runtime_error("cpp_may_fail: empty input");
    }
    return static_cast<long>(s.size());
}
