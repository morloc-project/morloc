#ifndef __LABELED_HPP__
#define __LABELED_HPP__

#include "mlc_tensor.hpp"
#include <vector>
#include <cstdint>

// Create a vector of given length, filled with 1.0, 2.0, ...
std::vector<double> makeVec(int n) {
    std::vector<double> v(n);
    for (int i = 0; i < n; i++) v[i] = (double)(i + 1);
    return v;
}

// Create an m x n matrix filled with row*10 + col
mlc::Tensor2<double> makeMat(int m, int n) {
    mlc::Tensor2<double> mat(std::array<int64_t, 2>{(int64_t)m, (int64_t)n});
    auto mv = mat.view();
    for (int i = 0; i < m; i++)
        for (int j = 0; j < n; j++)
            mv(i, j) = (double)(i * 10 + j);
    return mat;
}

// Identity: return the vector unchanged
std::vector<double> idVec(const std::vector<double>& v) {
    return v;
}

#endif
