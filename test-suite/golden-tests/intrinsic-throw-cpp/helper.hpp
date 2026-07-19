#pragma once

#include <string>

static inline long cpp_len(const std::string& s) {
    return static_cast<long>(s.size());
}

static inline std::string cpp_show_len(const std::string& s) {
    return std::to_string(s.size());
}
