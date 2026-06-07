#ifndef __SRC_HPP__
#define __SRC_HPP__
#include <deque>
#include <cstdint>

// Deque variants -- morloc Deque maps to std::deque<$1> on the C++
// side. The Packable (List a) (Deque a) instance handles the
// std::vector<->std::deque conversion at the wire boundary.

std::deque<int32_t> cppMakeI32() {
    std::deque<int32_t> v;
    for (int i = 0; i < 8; i++) v.push_back((int32_t)(i + 10));
    return v;
}
int cppSumI32(const std::deque<int32_t>& v) {
    int s = 0; for (int32_t x : v) s += x; return s;
}

std::deque<int64_t> cppMakeI64() {
    std::deque<int64_t> v;
    for (int i = 0; i < 8; i++) v.push_back((int64_t)(i + 10));
    return v;
}
int cppSumI64(const std::deque<int64_t>& v) {
    int s = 0; for (int64_t x : v) s += (int)x; return s;
}

std::deque<float> cppMakeF32() {
    std::deque<float> v;
    v.push_back(1.5f); v.push_back(2.5f); v.push_back(3.5f);
    return v;
}
double cppSumF32(const std::deque<float>& v) {
    double s = 0; for (float x : v) s += x; return s;
}

std::deque<double> cppMakeF64() {
    std::deque<double> v;
    v.push_back(1.5); v.push_back(2.5); v.push_back(3.5);
    return v;
}
double cppSumF64(const std::deque<double>& v) {
    double s = 0; for (double x : v) s += x; return s;
}

#endif
