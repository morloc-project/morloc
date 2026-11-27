#ifndef __TENSOR_COMPREHENSIVE_SRC_HPP__
#define __TENSOR_COMPREHENSIVE_SRC_HPP__

#include "tensor.hpp"
#include <cstdint>
#include <vector>

// ---------------------------------------------------------------------------
// Round-trip baselines: row-major fill, distinct sums per rank.
// ---------------------------------------------------------------------------

// Matrix 3x4 Real, values 1..12, sum = 78
inline mlc::Tensor2<double> cppMakeMat34() {
    mlc::Tensor2<double> m({3, 4});
    for (size_t k = 0; k < m.size(); k++) m[k] = (double)(k + 1);
    return m;
}
inline double cppSumMat34(const mlc::Tensor2<double>& m) {
    double s = 0;
    for (size_t k = 0; k < m.size(); k++) s += m.data()[k];
    return s;
}

// Tensor3 2x3x4 Real, values 0..23, sum = 276
inline mlc::Tensor3<double> cppMakeT3() {
    mlc::Tensor3<double> t({2, 3, 4});
    for (size_t k = 0; k < t.size(); k++) t[k] = (double)k;
    return t;
}
inline double cppSumT3(const mlc::Tensor3<double>& t) {
    double s = 0;
    for (size_t k = 0; k < t.size(); k++) s += t.data()[k];
    return s;
}

// Tensor4 2x3x2x2 Real, values 0..23, sum = 276
inline mlc::Tensor4<double> cppMakeT4() {
    mlc::Tensor4<double> t({2, 3, 2, 2});
    for (size_t k = 0; k < t.size(); k++) t[k] = (double)k;
    return t;
}
inline double cppSumT4(const mlc::Tensor4<double>& t) {
    double s = 0;
    for (size_t k = 0; k < t.size(); k++) s += t.data()[k];
    return s;
}

// Tensor5 2x2x2x2x2 Real, values 0..31, sum = 31*32/2 = 496
inline mlc::Tensor5<double> cppMakeT5() {
    mlc::Tensor5<double> t({2, 2, 2, 2, 2});
    for (size_t k = 0; k < t.size(); k++) t[k] = (double)k;
    return t;
}
inline double cppSumT5(const mlc::Tensor5<double>& t) {
    double s = 0;
    for (size_t k = 0; k < t.size(); k++) s += t.data()[k];
    return s;
}

// ---------------------------------------------------------------------------
// matmul probe: A (2x3) * B (3x2) -> C (2x2), sum = 163
// A = [[1,2,3],[4,5,6]]; B = [[1,2],[3,4],[5,6]]
// C = [[22,28],[49,64]]; sum = 163
// ---------------------------------------------------------------------------
inline mlc::Tensor2<double> cppMakeA23() {
    mlc::Tensor2<double> m({2, 3});
    for (size_t k = 0; k < m.size(); k++) m[k] = (double)(k + 1);
    return m;
}
inline mlc::Tensor2<double> cppMakeB32() {
    mlc::Tensor2<double> m({3, 2});
    for (size_t k = 0; k < m.size(); k++) m[k] = (double)(k + 1);
    return m;
}
inline double cppSumMat22(const mlc::Tensor2<double>& m) {
    double s = 0;
    for (size_t k = 0; k < m.size(); k++) s += m.data()[k];
    return s;
}

// ---------------------------------------------------------------------------
// Corner extraction: positions (0,0,0), (1,0,0), (0,2,3), (1,2,3) on a
// Tensor3 2x3x4 with row-major fill 0..23. Expected: [0, 12, 11, 23].
// ---------------------------------------------------------------------------
inline std::vector<double> cppCornersT3(const mlc::Tensor3<double>& t) {
    std::vector<double> r;
    // row-major: index = i*(d2*d3) + j*d3 + k
    r.push_back(t.data()[0*12 + 0*4 + 0]);  // (0,0,0)
    r.push_back(t.data()[1*12 + 0*4 + 0]);  // (1,0,0)
    r.push_back(t.data()[0*12 + 2*4 + 3]);  // (0,2,3)
    r.push_back(t.data()[1*12 + 2*4 + 3]);  // (1,2,3)
    return r;
}

// ---------------------------------------------------------------------------
// Element types on Matrix 4x4, values 1..16, sum = 136.
// ---------------------------------------------------------------------------
inline mlc::Tensor2<int32_t> cppMakeMatI32() {
    mlc::Tensor2<int32_t> m({4, 4});
    for (size_t k = 0; k < m.size(); k++) m[k] = (int32_t)(k + 1);
    return m;
}
inline int64_t cppSumMatI32(const mlc::Tensor2<int32_t>& m) {
    int64_t s = 0;
    for (size_t k = 0; k < m.size(); k++) s += (int64_t)m.data()[k];
    return s;
}

inline mlc::Tensor2<int64_t> cppMakeMatI64() {
    mlc::Tensor2<int64_t> m({4, 4});
    for (size_t k = 0; k < m.size(); k++) m[k] = (int64_t)(k + 1);
    return m;
}
inline int64_t cppSumMatI64(const mlc::Tensor2<int64_t>& m) {
    int64_t s = 0;
    for (size_t k = 0; k < m.size(); k++) s += m.data()[k];
    return s;
}

inline mlc::Tensor2<float> cppMakeMatF32() {
    mlc::Tensor2<float> m({4, 4});
    for (size_t k = 0; k < m.size(); k++) m[k] = (float)(k + 1);
    return m;
}
inline double cppSumMatF32(const mlc::Tensor2<float>& m) {
    double s = 0;
    for (size_t k = 0; k < m.size(); k++) s += (double)m.data()[k];
    return s;
}

// ---------------------------------------------------------------------------
// Big-payload producers (all 1.0). The sums tell us if any elements were
// lost, in addition to triggering SHM routing via size.
//   Matrix 200x100 = 20000 doubles = 160 KB,  sum = 20000
//   Tensor3 50x40x30 = 60000 doubles = 480 KB, sum = 60000
//   Tensor5 8^5 = 32768 doubles = 256 KB,      sum = 32768
// ---------------------------------------------------------------------------
inline mlc::Tensor2<double> cppMakeMatBig() {
    mlc::Tensor2<double> m({200, 100});
    for (size_t k = 0; k < m.size(); k++) m[k] = 1.0;
    return m;
}
inline mlc::Tensor3<double> cppMakeT3Big() {
    mlc::Tensor3<double> t({50, 40, 30});
    for (size_t k = 0; k < t.size(); k++) t[k] = 1.0;
    return t;
}
inline mlc::Tensor5<double> cppMakeT5Big() {
    mlc::Tensor5<double> t({8, 8, 8, 8, 8});
    for (size_t k = 0; k < t.size(); k++) t[k] = 1.0;
    return t;
}

// ---------------------------------------------------------------------------
// Edge cases: 1x1 matrix and 1x1x1 tensor.
// ---------------------------------------------------------------------------
inline mlc::Tensor2<double> cppMakeUnitMat() {
    mlc::Tensor2<double> m({1, 1});
    m[0] = 7.0;
    return m;
}
inline double cppSumUnitMat(const mlc::Tensor2<double>& m) {
    return m.size() > 0 ? m.data()[0] : 0.0;
}
inline mlc::Tensor3<double> cppMakeUnitT3() {
    mlc::Tensor3<double> t({1, 1, 1});
    t[0] = 9.0;
    return t;
}
inline double cppSumUnitT3(const mlc::Tensor3<double>& t) {
    return t.size() > 0 ? t.data()[0] : 0.0;
}

// ---------------------------------------------------------------------------
// Polymorphic-in-shape sum helpers for literal-input tests.
// ---------------------------------------------------------------------------
inline double cppSumMatPoly(const mlc::Tensor2<double>& m) {
    double s = 0;
    for (size_t k = 0; k < m.size(); k++) s += m.data()[k];
    return s;
}
inline int64_t cppSumMatI32Poly(const mlc::Tensor2<int32_t>& m) {
    int64_t s = 0;
    for (size_t k = 0; k < m.size(); k++) s += (int64_t)m.data()[k];
    return s;
}
inline double cppSumT3Poly(const mlc::Tensor3<double>& t) {
    double s = 0;
    for (size_t k = 0; k < t.size(); k++) s += t.data()[k];
    return s;
}

#endif
