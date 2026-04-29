#ifndef __SRC_HPP__
#define __SRC_HPP__
#include "mlc_tensor.hpp"
#include <vector>
#include <cstdint>
#include <cmath>

// 1D vector: [1.0, 2.0, 3.0, 4.0, 5.0]
std::vector<double> makeVec() {
    std::vector<double> v(5);
    for (int i = 0; i < 5; i++) v[i] = (double)(i + 1);
    return v;
}

// 2D matrix: [[1..4],[5..8],[9..12]]
mlc::Tensor2<double> makeMat() {
    mlc::Tensor2<double> m({3, 4});
    auto mv = m.view();
    for (int i = 0; i < 3; i++)
        for (int j = 0; j < 4; j++)
            mv(i, j) = (double)(i * 4 + j + 1);
    return m;
}

// 3D: [[[0..3],[4..7],[8..11]],[[12..15],[16..19],[20..23]]]
mlc::Tensor3<double> make3d() {
    mlc::Tensor3<double> t({2, 3, 4});
    for (size_t k = 0; k < 24; k++) t[k] = (double)k;
    return t;
}

// Edge: empty
std::vector<double> makeEmpty() {
    return std::vector<double>();
}

// Edge: single element
std::vector<double> makeSingle() {
    return std::vector<double>{42.0};
}

// Edge: 1x1 matrix
mlc::Tensor2<double> makeOneByOne() {
    mlc::Tensor2<double> m({1, 1});
    m.view()(0, 0) = 99.0;
    return m;
}

// Int32 elements
std::vector<int32_t> makeIntVec() {
    return std::vector<int32_t>{-10, 0, 42, 2147483647};
}

// Bool elements
std::vector<uint8_t> makeBoolVec() {
    return std::vector<uint8_t>{1, 0, 1};
}

// Float32 elements
std::vector<float> makeF32Vec() {
    return std::vector<float>{1.5f, -2.5f, 0.0f};
}

// Sum operations
double sumMat(const mlc::Tensor2<double>& m) {
    double s = 0;
    for (size_t k = 0; k < m.size(); k++) s += m.data()[k];
    return s;
}

double sumVec(const std::vector<double>& v) {
    double s = 0;
    for (double x : v) s += x;
    return s;
}

double sum3d(const mlc::Tensor3<double>& t) {
    double s = 0;
    for (size_t k = 0; k < t.size(); k++) s += t.data()[k];
    return s;
}

// Tensor -> Tensor: scale each element
std::vector<double> scaleVec(const std::vector<double>& v, double factor) {
    std::vector<double> result(v.size());
    for (size_t i = 0; i < v.size(); i++)
        result[i] = v[i] * factor;
    return result;
}

// Tensor + Tensor: element-wise add
std::vector<double> addVecs(const std::vector<double>& a, const std::vector<double>& b) {
    std::vector<double> result(a.size());
    for (size_t i = 0; i < a.size(); i++)
        result[i] = a[i] + b[i];
    return result;
}

// Large vector (800KB at float64 -- exceeds inline threshold)
std::vector<double> makeLargeVec() {
    std::vector<double> v(100000);
    for (int i = 0; i < 100000; i++) v[i] = (double)i;
    return v;
}

// Sum of 0+1+...+99999 = 4999950000
double checkLargeVec(const std::vector<double>& v) {
    double s = 0;
    for (double x : v) s += x;
    return s;
}

#endif
