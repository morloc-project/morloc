#ifndef __SRC_HPP__
#define __SRC_HPP__
#include "mlc_tensor.hpp"
#include <cmath>

// 1D vector: [1.0, 2.0, 3.0, 4.0, 5.0]
mlc::Tensor1<double> makeVec() {
    mlc::Tensor1<double> v({5});
    for (int i = 0; i < 5; i++) v[i] = (double)(i + 1);
    return v;
}

// 2D matrix: [[1..4],[5..8],[9..12]]
mlc::Tensor2<double> makeMat() {
    mlc::Tensor2<double> m({3, 4});
    for (int i = 0; i < 3; i++)
        for (int j = 0; j < 4; j++)
            m(i, j) = (double)(i * 4 + j + 1);
    return m;
}

// 3D: [[[0..3],[4..7],[8..11]],[[12..15],[16..19],[20..23]]]
mlc::Tensor3<double> make3d() {
    mlc::Tensor3<double> t({2, 3, 4});
    for (size_t k = 0; k < 24; k++) t[k] = (double)k;
    return t;
}

// Edge: empty
mlc::Tensor1<double> makeEmpty() {
    return mlc::Tensor1<double>({0});
}

// Edge: single element
mlc::Tensor1<double> makeSingle() {
    mlc::Tensor1<double> v({1});
    v[0] = 42.0;
    return v;
}

// Edge: 1x1 matrix
mlc::Tensor2<double> makeOneByOne() {
    mlc::Tensor2<double> m({1, 1});
    m(0, 0) = 99.0;
    return m;
}

// Int elements
mlc::Tensor1<int> makeIntVec() {
    mlc::Tensor1<int> v({4});
    v[0] = -10; v[1] = 0; v[2] = 42; v[3] = 2147483647;
    return v;
}

// Bool elements
mlc::Tensor1<uint8_t> makeBoolVec() {
    mlc::Tensor1<uint8_t> v({3});
    v[0] = 1; v[1] = 0; v[2] = 1;
    return v;
}

// Float32 elements
mlc::Tensor1<float> makeF32Vec() {
    mlc::Tensor1<float> v({3});
    v[0] = 1.5f; v[1] = -2.5f; v[2] = 0.0f;
    return v;
}

// Sum operations
double sumMat(const mlc::Tensor2<double>& m) {
    double s = 0;
    for (size_t k = 0; k < m.size(); k++) s += m.data()[k];
    return s;
}

double sumVec(const mlc::Tensor1<double>& v) {
    double s = 0;
    for (size_t k = 0; k < v.size(); k++) s += v.data()[k];
    return s;
}

double sum3d(const mlc::Tensor3<double>& t) {
    double s = 0;
    for (size_t k = 0; k < t.size(); k++) s += t.data()[k];
    return s;
}

// Tensor -> Tensor: scale each element
mlc::Tensor1<double> scaleVec(const mlc::Tensor1<double>& v, double factor) {
    mlc::Tensor1<double> result({v.shape(0)});
    for (size_t i = 0; i < v.size(); i++)
        result[i] = v.data()[i] * factor;
    return result;
}

// Tensor + Tensor: element-wise add
mlc::Tensor1<double> addVecs(const mlc::Tensor1<double>& a, const mlc::Tensor1<double>& b) {
    mlc::Tensor1<double> result({a.shape(0)});
    for (size_t i = 0; i < a.size(); i++)
        result[i] = a.data()[i] + b.data()[i];
    return result;
}

// Large vector (800KB at float64 -- exceeds inline threshold)
mlc::Tensor1<double> makeLargeVec() {
    mlc::Tensor1<double> v({100000});
    for (int i = 0; i < 100000; i++) v[i] = (double)i;
    return v;
}

// Sum of 0+1+...+99999 = 4999950000
double checkLargeVec(const mlc::Tensor1<double>& v) {
    double s = 0;
    for (size_t k = 0; k < v.size(); k++) s += v.data()[k];
    return s;
}

#endif
