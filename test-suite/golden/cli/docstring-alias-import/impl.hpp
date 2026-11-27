#pragma once
#include <vector>
#include <string>
#include <sstream>

inline std::vector<int> mk(int n) {
    std::vector<int> xs;
    for (int i = 0; i < n; ++i) xs.push_back(i);
    return xs;
}
inline std::vector<int> echo(std::vector<int> xs) { return xs; }
inline std::string renderInts(std::vector<int> xs) {
    std::ostringstream o;
    for (size_t i = 0; i < xs.size(); ++i) { if (i) o << ","; o << xs[i]; }
    o << "\n";
    return o.str();
}
