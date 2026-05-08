#ifndef __SRC_HPP__
#define __SRC_HPP__
#include <vector>
#include <deque>
#include <cstdint>

// Array variants -- morloc Array maps to std::vector

std::vector<int32_t> cppMakeI32() {
    std::vector<int32_t> v(8);
    for (int i = 0; i < 8; i++) v[i] = (int32_t)(i + 10);
    return v;
}
int cppSumI32(const std::vector<int32_t>& v) {
    int s = 0; for (int32_t x : v) s += x; return s;
}

std::vector<int64_t> cppMakeI64() {
    std::vector<int64_t> v(8);
    for (int i = 0; i < 8; i++) v[i] = (int64_t)(i + 10);
    return v;
}
int cppSumI64(const std::vector<int64_t>& v) {
    int s = 0; for (int64_t x : v) s += (int)x; return s;
}

std::vector<float> cppMakeF32() {
    return std::vector<float>{1.5f, 2.5f, 3.5f};
}
double cppSumF32(const std::vector<float>& v) {
    double s = 0; for (float x : v) s += x; return s;
}

std::vector<double> cppMakeF64() {
    return std::vector<double>{1.5, 2.5, 3.5};
}
double cppSumF64(const std::vector<double>& v) {
    double s = 0; for (double x : v) s += x; return s;
}

// Deque variants -- morloc Deque maps to std::deque

std::deque<int32_t> cppMakeI32D() {
    std::deque<int32_t> v;
    for (int i = 0; i < 8; i++) v.push_back((int32_t)(i + 10));
    return v;
}
int cppSumI32D(const std::deque<int32_t>& v) {
    int s = 0; for (int32_t x : v) s += x; return s;
}

std::deque<float> cppMakeF32D() {
    std::deque<float> v;
    v.push_back(1.5f); v.push_back(2.5f); v.push_back(3.5f);
    return v;
}
double cppSumF32D(const std::deque<float>& v) {
    double s = 0; for (float x : v) s += x; return s;
}

#endif
