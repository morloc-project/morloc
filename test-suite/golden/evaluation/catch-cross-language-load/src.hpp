#pragma once
#include <vector>

inline std::vector<double> cppMkVec4() {
    return {1.5, 2.5, 3.5, 4.5};
}

inline double cppSumVec4(const std::vector<double>& v) {
    double s = 0.0;
    for (double x : v) s += x;
    return s;
}
