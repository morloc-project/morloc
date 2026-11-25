#pragma once

#include <string>
#include <stdexcept>

static inline long cpp_fail(const std::string& s) {
    if (s.empty()) {
        throw std::runtime_error("cpp_fail: empty input");
    }
    return static_cast<long>(s.size());
}
