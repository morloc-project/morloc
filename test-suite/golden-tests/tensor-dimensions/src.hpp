#ifndef __SRC_HPP__
#define __SRC_HPP__
#include "mlc_tensor.hpp"

mlc::Tensor<int32_t, 4> cppMake4d() {
    int64_t dims[4] = {2, 3, 2, 2};
    mlc::Tensor<int32_t, 4> t(dims);
    for (size_t i = 0; i < t.size(); i++) t[i] = (int32_t)i;
    return t;
}

mlc::Tensor<int32_t, 5> cppMake5d() {
    int64_t dims[5] = {2, 2, 2, 3, 2};
    mlc::Tensor<int32_t, 5> t(dims);
    for (size_t i = 0; i < t.size(); i++) t[i] = (int32_t)i;
    return t;
}

int cppSum1d(const mlc::Tensor1<int32_t>& t) {
    int32_t s = 0; for (size_t i = 0; i < t.size(); i++) s += t.data()[i]; return s;
}
int cppSum2d(const mlc::Tensor2<int32_t>& t) {
    int32_t s = 0; for (size_t i = 0; i < t.size(); i++) s += t.data()[i]; return s;
}
int cppSum3d(const mlc::Tensor3<int32_t>& t) {
    int32_t s = 0; for (size_t i = 0; i < t.size(); i++) s += t.data()[i]; return s;
}
int cppSum4d(const mlc::Tensor<int32_t, 4>& t) {
    int32_t s = 0; for (size_t i = 0; i < t.size(); i++) s += t.data()[i]; return s;
}
int cppSum5d(const mlc::Tensor<int32_t, 5>& t) {
    int32_t s = 0; for (size_t i = 0; i < t.size(); i++) s += t.data()[i]; return s;
}

#endif
