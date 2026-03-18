#ifndef __SRC_HPP__
#define __SRC_HPP__

#include "mlc_tensor.hpp"
#include <cstring>

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

mlc::Tensor1<double> onesV3() {
    mlc::Tensor1<double> t({3});
    for (size_t i = 0; i < t.size(); i++) t[i] = 1.0;
    return t;
}

mlc::Tensor1<double> onesV4() {
    mlc::Tensor1<double> t({4});
    for (size_t i = 0; i < t.size(); i++) t[i] = 1.0;
    return t;
}

mlc::Tensor1<double> onesV5() {
    mlc::Tensor1<double> t({5});
    for (size_t i = 0; i < t.size(); i++) t[i] = 1.0;
    return t;
}

mlc::Tensor1<double> onesV10() {
    mlc::Tensor1<double> t({10});
    for (size_t i = 0; i < t.size(); i++) t[i] = 1.0;
    return t;
}

mlc::Tensor2<double> eye33() {
    mlc::Tensor2<double> t({3, 3});
    for (size_t i = 0; i < t.size(); i++) t[i] = 0.0;
    for (int i = 0; i < 3; i++) t(i, i) = 1.0;
    return t;
}

mlc::Tensor2<double> eye55() {
    mlc::Tensor2<double> t({5, 5});
    for (size_t i = 0; i < t.size(); i++) t[i] = 0.0;
    for (int i = 0; i < 5; i++) t(i, i) = 1.0;
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
    for (int64_t i = 0; i < m; i++)
        for (int64_t j = 0; j < n; j++)
            r(j, i) = a(i, j);
    return r;
}

mlc::Tensor2<double> matmul2d(
    const mlc::Tensor2<double>& a,
    const mlc::Tensor2<double>& b
) {
    int64_t m = a.shape()[0], k = a.shape()[1], n = b.shape()[1];
    mlc::Tensor2<double> r({m, n});
    for (size_t i = 0; i < r.size(); i++) r[i] = 0.0;
    for (int64_t i = 0; i < m; i++)
        for (int64_t j = 0; j < n; j++)
            for (int64_t l = 0; l < k; l++)
                r(i, j) += a(i, l) * b(l, j);
    return r;
}

double dot1d(
    const mlc::Tensor1<double>& a,
    const mlc::Tensor1<double>& b
) {
    double s = 0;
    for (size_t i = 0; i < a.size(); i++)
        s += a.data()[i] * b.data()[i];
    return s;
}

double tsum1d(const mlc::Tensor1<double>& a) {
    double s = 0;
    for (size_t i = 0; i < a.size(); i++) s += a.data()[i];
    return s;
}

double trace2d(const mlc::Tensor2<double>& a) {
    int64_t n = a.shape()[0];
    double s = 0;
    for (int64_t i = 0; i < n; i++) s += a(i, i);
    return s;
}

mlc::Tensor1<double> diag2d(const mlc::Tensor2<double>& a) {
    int64_t n = a.shape()[0];
    mlc::Tensor1<double> r({n});
    for (int64_t i = 0; i < n; i++) r.data()[i] = a(i, i);
    return r;
}

mlc::Tensor2<double> diagMat1d(const mlc::Tensor1<double>& a) {
    int64_t n = a.shape()[0];
    mlc::Tensor2<double> r({n, n});
    for (size_t i = 0; i < r.size(); i++) r[i] = 0.0;
    for (int64_t i = 0; i < n; i++) r(i, i) = a.data()[i];
    return r;
}

mlc::Tensor1<double> flatten2d(const mlc::Tensor2<double>& a) {
    int64_t total = (int64_t)a.size();
    mlc::Tensor1<double> r({total});
    memcpy(r.data(), a.data(), (size_t)total * sizeof(double));
    return r;
}

mlc::Tensor2<double> outer1d(
    const mlc::Tensor1<double>& a,
    const mlc::Tensor1<double>& b
) {
    int64_t m = a.shape()[0], n = b.shape()[0];
    mlc::Tensor2<double> r({m, n});
    for (int64_t i = 0; i < m; i++)
        for (int64_t j = 0; j < n; j++)
            r(i, j) = a.data()[i] * b.data()[j];
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
    for (int64_t ia = 0; ia < ma; ia++)
        for (int64_t ja = 0; ja < na; ja++)
            for (int64_t ib = 0; ib < mb; ib++)
                for (int64_t jb = 0; jb < nb; jb++)
                    r(ia * mb + ib, ja * nb + jb) = a(ia, ja) * b(ib, jb);
    return r;
}

mlc::Tensor2<double> slice2d(
    int rows, int cols,
    const mlc::Tensor2<double>& a
) {
    mlc::Tensor2<double> r({(int64_t)rows, (int64_t)cols});
    for (int i = 0; i < rows; i++)
        for (int j = 0; j < cols; j++)
            r(i, j) = a(i, j);
    return r;
}

mlc::Tensor1<double> ttake1d(int n, const mlc::Tensor1<double>& a) {
    mlc::Tensor1<double> r({(int64_t)n});
    memcpy(r.data(), a.data(), (size_t)n * sizeof(double));
    return r;
}

#endif
