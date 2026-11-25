#pragma once
#include <cstdint>
#include <string>
inline int64_t packDate(const std::string& s) { return std::stoll(s); }
inline std::string unpackDate(int64_t d) { return std::to_string(d); }
inline int64_t dateOf(int64_t x) { return x; }
