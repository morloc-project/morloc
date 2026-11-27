#pragma once
#include <string>
#include <vector>
inline std::string cpp_join(const std::vector<std::string>& xs) {
    std::string out;
    for (size_t i = 0; i < xs.size(); i++) { if (i) out += "+"; out += xs[i]; }
    return out;
}
