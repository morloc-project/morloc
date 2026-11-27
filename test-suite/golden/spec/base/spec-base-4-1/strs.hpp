#pragma once
#include <string>
inline std::string good(int) { return std::string("caf\xc3\xa9"); }
inline std::string bad(int) { return std::string("ab\xff" "cd"); }
