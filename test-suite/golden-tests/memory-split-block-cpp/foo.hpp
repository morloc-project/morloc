#ifndef MORLOC_MEMORY_SPLIT_BLOCK_HPP
#define MORLOC_MEMORY_SPLIT_BLOCK_HPP

#include <string>

bool cNotBool(bool x) {
    return !x;
}

std::string cAppendStr(const std::string& a, const std::string& b) {
    return a + b;
}

std::string cBoolToStr(bool x) {
    return x ? "true" : "false";
}

int cStrLen(const std::string& s) {
    return static_cast<int>(s.size());
}

#endif
