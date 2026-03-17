#ifndef __LABELED_HPP__
#define __LABELED_HPP__

#include "mlc_tensor.hpp"

// Create a vector of given length, filled with 1.0, 2.0, ...
mlc::Tensor1<double> makeVec(int n) {
    mlc::Tensor1<double> v({(int64_t)n});
    for (int i = 0; i < n; i++) v[i] = (double)(i + 1);
    return v;
}

// Create an m x n matrix filled with row*10 + col
mlc::Tensor2<double> makeMat(int m, int n) {
    mlc::Tensor2<double> mat({(int64_t)m, (int64_t)n});
    for (int i = 0; i < m; i++)
        for (int j = 0; j < n; j++)
            mat(i, j) = (double)(i * 10 + j);
    return mat;
}

// Identity: return the vector unchanged (must clone since Tensor has no copy ctor)
mlc::Tensor1<double> idVec(const mlc::Tensor1<double>& v) {
    mlc::Tensor1<double> out({v.shape(0)});
    for (size_t i = 0; i < v.size(); i++) out[i] = v.data()[i];
    return out;
}

#endif
