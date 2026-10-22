#pragma once
#include <stdexcept>

// Throws when n == 0; otherwise returns 10.0 / n. Used to exercise
// the C++ debug-trace wrap with a deterministic exception.
inline double cppThrowAtZero(double n) {
    if (n == 0.0) {
        throw std::invalid_argument("cppThrowAtZero: n must be non-zero");
    }
    return 10.0 / n;
}
