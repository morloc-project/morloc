#pragma once
#include <vector>
inline double cpp_sum(const std::vector<double>& xs) {
    double s = 0;
    for (double x : xs) s += x;
    return s;
}
