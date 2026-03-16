#ifndef __SRC_HPP__
#define __SRC_HPP__
#include "mlc_tensor.hpp"
#include <cstdint>

// --- 2D Real (3x4, values 1..12) ---

mlc::Tensor2<double> cppMakeMat() {
    mlc::Tensor2<double> m({3, 4});
    for (int i = 0; i < 3; i++)
        for (int j = 0; j < 4; j++)
            m(i, j) = (double)(i * 4 + j + 1);
    return m;
}

double cppSumMat(const mlc::Tensor2<double>& m) {
    double s = 0;
    for (size_t k = 0; k < m.size(); k++) s += m.data()[k];
    return s;
}

// --- 3D Real (2x3x4, values 0..23) ---

mlc::Tensor3<double> cppMake3d() {
    mlc::Tensor3<double> t({2, 3, 4});
    for (size_t k = 0; k < 24; k++) t[k] = (double)k;
    return t;
}

double cppSum3d(const mlc::Tensor3<double>& t) {
    double s = 0;
    for (size_t k = 0; k < t.size(); k++) s += t.data()[k];
    return s;
}

std::vector<double> cppGetCorners3d(const mlc::Tensor3<double>& t) {
    std::vector<double> result;
    result.push_back(t.data()[0*12 + 0*4 + 0]);  // t(0,0,0)
    result.push_back(t.data()[1*12 + 0*4 + 0]);  // t(1,0,0)
    result.push_back(t.data()[0*12 + 1*4 + 0]);  // t(0,1,0)
    result.push_back(t.data()[1*12 + 2*4 + 3]);  // t(1,2,3)
    return result;
}

// --- 1D Real (10 elements, values 1..10) ---

mlc::Tensor1<double> cppMakeVec() {
    mlc::Tensor1<double> v({10});
    for (int i = 0; i < 10; i++) v[i] = (double)(i + 1);
    return v;
}

double cppSumVec(const mlc::Tensor1<double>& v) {
    double s = 0;
    for (size_t k = 0; k < v.size(); k++) s += v.data()[k];
    return s;
}

// --- 4D Real (2x3x2x2, values 0..23) ---

mlc::Tensor4<double> cppMake4d() {
    mlc::Tensor4<double> t({2, 3, 2, 2});
    for (size_t k = 0; k < 24; k++) t[k] = (double)k;
    return t;
}

double cppSum4d(const mlc::Tensor4<double>& t) {
    double s = 0;
    for (size_t k = 0; k < t.size(); k++) s += t.data()[k];
    return s;
}

// --- 1D Int (8 elements, values 10..17) ---

mlc::Tensor1<int> cppMakeIntVec() {
    mlc::Tensor1<int> v({8});
    for (int i = 0; i < 8; i++) v[i] = i + 10;
    return v;
}

int cppSumIntVec(const mlc::Tensor1<int>& v) {
    int s = 0;
    for (size_t k = 0; k < v.size(); k++) s += v.data()[k];
    return s;
}

// --- 1D Bool (6 elements: T,F,T,T,F,T) ---

mlc::Tensor1<bool> cppMakeBoolVec() {
    mlc::Tensor1<bool> v({6});
    v[0] = 1; v[1] = 0; v[2] = 1; v[3] = 1; v[4] = 0; v[5] = 1;
    return v;
}

int cppCountTrue(const mlc::Tensor1<bool>& v) {
    int count = 0;
    for (size_t k = 0; k < v.size(); k++) {
        if (v.data()[k]) count++;
    }
    return count;
}

// --- Empty tensor (0 elements) ---

mlc::Tensor1<double> cppMakeEmpty() {
    return mlc::Tensor1<double>({0});
}

double cppSumEmpty(const mlc::Tensor1<double>& v) {
    double s = 0;
    for (size_t k = 0; k < v.size(); k++) s += v.data()[k];
    return s;
}

// --- Single element (value 42) ---

mlc::Tensor1<double> cppMakeSingle() {
    mlc::Tensor1<double> v({1});
    v[0] = 42.0;
    return v;
}

double cppSumSingle(const mlc::Tensor1<double>& v) {
    double s = 0;
    for (size_t k = 0; k < v.size(); k++) s += v.data()[k];
    return s;
}

// --- Large tensor (5000 doubles = 40KB) ---

mlc::Tensor1<double> cppMakeLarge() {
    mlc::Tensor1<double> v({5000});
    for (int i = 0; i < 5000; i++) v[i] = (double)i;
    return v;
}

double cppSumLarge(const mlc::Tensor1<double>& v) {
    double s = 0;
    for (size_t k = 0; k < v.size(); k++) s += v.data()[k];
    return s;
}

// --- Very large tensor (50000 doubles = 400KB, crosses SHM threshold) ---

mlc::Tensor1<double> cppMakeHuge() {
    mlc::Tensor1<double> v({50000});
    for (int i = 0; i < 50000; i++) v[i] = (double)i;
    return v;
}

double cppSumHuge(const mlc::Tensor1<double>& v) {
    double s = 0;
    for (size_t k = 0; k < v.size(); k++) s += v.data()[k];
    return s;
}

#endif
