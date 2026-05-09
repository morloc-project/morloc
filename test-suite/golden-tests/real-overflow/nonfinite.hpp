#ifndef MORLOC_NONFINITE_HPP
#define MORLOC_NONFINITE_HPP

#include <cmath>
#include <limits>

// IEEE-754 arithmetic on Real (double). C++ produces non-finite results
// per IEEE-754 with default compiler flags; the morloc wire format then
// emits them as the canonical lowercase strings.

inline double cppInf(double) {
    return std::numeric_limits<double>::infinity();
}

inline double cppNegInf(double) {
    return -std::numeric_limits<double>::infinity();
}

inline double cppNaN(double) {
    return std::numeric_limits<double>::quiet_NaN();
}

inline double add(double a, double b) { return a + b; }
inline double sub(double a, double b) { return a - b; }
inline double mul(double a, double b) { return a * b; }
inline double neg(double a) { return -a; }

#endif
