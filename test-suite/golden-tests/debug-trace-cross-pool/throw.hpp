#pragma once
#include <stdexcept>

inline double cppInnerThrow(double n) {
    if (n == 0.0) {
        throw std::invalid_argument("cppInnerThrow: divisor was zero");
    }
    return 100.0 / n;
}
