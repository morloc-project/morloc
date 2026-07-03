#ifndef __SRC_HPP__
#define __SRC_HPP__

#include "tensor-cpp/tensor.hpp"
#include <vector>
#include <cstdint>
#include <cstring>

// Matrix maps to mlc::Tensor2; Vector maps to std::vector in C++.

// --- Tensor constructors (specific sizes) ---

mlc::Tensor2<double> ones34() {
    mlc::Tensor2<double> t({3, 4});
    for (size_t i = 0; i < t.size(); i++) t[i] = 1.0;
    return t;
}

mlc::Tensor2<double> ones23() {
    mlc::Tensor2<double> t({2, 3});
    for (size_t i = 0; i < t.size(); i++) t[i] = 1.0;
    return t;
}

mlc::Tensor2<double> ones32() {
    mlc::Tensor2<double> t({3, 2});
    for (size_t i = 0; i < t.size(); i++) t[i] = 1.0;
    return t;
}

mlc::Tensor2<double> ones22() {
    mlc::Tensor2<double> t({2, 2});
    for (size_t i = 0; i < t.size(); i++) t[i] = 1.0;
    return t;
}

mlc::Tensor2<double> ones43() {
    mlc::Tensor2<double> t({4, 3});
    for (size_t i = 0; i < t.size(); i++) t[i] = 1.0;
    return t;
}

mlc::Tensor2<double> ones56() {
    mlc::Tensor2<double> t({5, 6});
    for (size_t i = 0; i < t.size(); i++) t[i] = 1.0;
    return t;
}

std::vector<double> onesV3()  { return std::vector<double>(3, 1.0); }
std::vector<double> onesV4()  { return std::vector<double>(4, 1.0); }
std::vector<double> onesV5()  { return std::vector<double>(5, 1.0); }
std::vector<double> onesV10() { return std::vector<double>(10, 1.0); }

mlc::Tensor2<double> eye33() {
    mlc::Tensor2<double> t({3, 3});
    for (size_t i = 0; i < t.size(); i++) t[i] = 0.0;
    auto tv = t.view();
    for (int i = 0; i < 3; i++) tv(i, i) = 1.0;
    return t;
}

mlc::Tensor2<double> eye55() {
    mlc::Tensor2<double> t({5, 5});
    for (size_t i = 0; i < t.size(); i++) t[i] = 0.0;
    auto tv = t.view();
    for (int i = 0; i < 5; i++) tv(i, i) = 1.0;
    return t;
}

// --- Operations ---

mlc::Tensor2<double> add2d(
    const mlc::Tensor2<double>& a,
    const mlc::Tensor2<double>& b
) {
    int64_t m = a.shape()[0], n = a.shape()[1];
    mlc::Tensor2<double> r({m, n});
    for (size_t i = 0; i < a.size(); i++)
        r[i] = a.data()[i] + b.data()[i];
    return r;
}

mlc::Tensor2<double> transpose2d(const mlc::Tensor2<double>& a) {
    int64_t m = a.shape()[0], n = a.shape()[1];
    mlc::Tensor2<double> r({n, m});
    auto av = a.view();
    auto rv = r.view();
    for (int64_t i = 0; i < m; i++)
        for (int64_t j = 0; j < n; j++)
            rv(j, i) = av(i, j);
    return r;
}

mlc::Tensor2<double> matmul2d(
    const mlc::Tensor2<double>& a,
    const mlc::Tensor2<double>& b
) {
    int64_t m = a.shape()[0], k = a.shape()[1], n = b.shape()[1];
    mlc::Tensor2<double> r({m, n});
    for (size_t i = 0; i < r.size(); i++) r[i] = 0.0;
    auto av = a.view();
    auto bv = b.view();
    auto rv = r.view();
    for (int64_t i = 0; i < m; i++)
        for (int64_t j = 0; j < n; j++)
            for (int64_t l = 0; l < k; l++)
                rv(i, j) += av(i, l) * bv(l, j);
    return r;
}

double dot1d(const std::vector<double>& a, const std::vector<double>& b) {
    double s = 0;
    for (size_t i = 0; i < a.size(); i++) s += a[i] * b[i];
    return s;
}

double tsum1d(const std::vector<double>& a) {
    double s = 0;
    for (double x : a) s += x;
    return s;
}

double trace2d(const mlc::Tensor2<double>& a) {
    int64_t n = a.shape()[0];
    auto av = a.view();
    double s = 0;
    for (int64_t i = 0; i < n; i++) s += av(i, i);
    return s;
}

std::vector<double> diag2d(const mlc::Tensor2<double>& a) {
    int64_t n = a.shape()[0];
    std::vector<double> r((size_t)n);
    auto av = a.view();
    for (int64_t i = 0; i < n; i++) r[i] = av(i, i);
    return r;
}

mlc::Tensor2<double> diagMat1d(const std::vector<double>& a) {
    int64_t n = (int64_t)a.size();
    mlc::Tensor2<double> r({n, n});
    for (size_t i = 0; i < r.size(); i++) r[i] = 0.0;
    auto rv = r.view();
    for (int64_t i = 0; i < n; i++) rv(i, i) = a[i];
    return r;
}

std::vector<double> flatten2d(const mlc::Tensor2<double>& a) {
    std::vector<double> r(a.size());
    memcpy(r.data(), a.data(), a.size() * sizeof(double));
    return r;
}

mlc::Tensor2<double> outer1d(
    const std::vector<double>& a,
    const std::vector<double>& b
) {
    int64_t m = (int64_t)a.size(), n = (int64_t)b.size();
    mlc::Tensor2<double> r({m, n});
    auto rv = r.view();
    for (int64_t i = 0; i < m; i++)
        for (int64_t j = 0; j < n; j++)
            rv(i, j) = a[i] * b[j];
    return r;
}

mlc::Tensor2<double> vstack2d(
    const mlc::Tensor2<double>& a,
    const mlc::Tensor2<double>& b
) {
    int64_t ma = a.shape()[0], mb = b.shape()[0], n = a.shape()[1];
    mlc::Tensor2<double> r({ma + mb, n});
    memcpy(r.data(), a.data(), (size_t)(ma * n) * sizeof(double));
    memcpy(r.data() + ma * n, b.data(), (size_t)(mb * n) * sizeof(double));
    return r;
}

mlc::Tensor2<double> hstack2d(
    const mlc::Tensor2<double>& a,
    const mlc::Tensor2<double>& b
) {
    int64_t m = a.shape()[0], na = a.shape()[1], nb = b.shape()[1];
    mlc::Tensor2<double> r({m, na + nb});
    for (int64_t i = 0; i < m; i++) {
        memcpy(r.data() + i * (na + nb),
               a.data() + i * na, (size_t)na * sizeof(double));
        memcpy(r.data() + i * (na + nb) + na,
               b.data() + i * nb, (size_t)nb * sizeof(double));
    }
    return r;
}

mlc::Tensor2<double> kron2d(
    const mlc::Tensor2<double>& a,
    const mlc::Tensor2<double>& b
) {
    int64_t ma = a.shape()[0], na = a.shape()[1];
    int64_t mb = b.shape()[0], nb = b.shape()[1];
    mlc::Tensor2<double> r({ma * mb, na * nb});
    auto av = a.view();
    auto bv = b.view();
    auto rv = r.view();
    for (int64_t ia = 0; ia < ma; ia++)
        for (int64_t ja = 0; ja < na; ja++)
            for (int64_t ib = 0; ib < mb; ib++)
                for (int64_t jb = 0; jb < nb; jb++)
                    rv(ia * mb + ib, ja * nb + jb) = av(ia, ja) * bv(ib, jb);
    return r;
}

mlc::Tensor2<double> slice2d(
    int rows, int cols,
    const mlc::Tensor2<double>& a
) {
    mlc::Tensor2<double> r({(int64_t)rows, (int64_t)cols});
    auto av = a.view();
    auto rv = r.view();
    for (int i = 0; i < rows; i++)
        for (int j = 0; j < cols; j++)
            rv(i, j) = av(i, j);
    return r;
}

std::vector<double> ttake1d(int n, const std::vector<double>& a) {
    return std::vector<double>(a.begin(), a.begin() + n);
}

#endif
