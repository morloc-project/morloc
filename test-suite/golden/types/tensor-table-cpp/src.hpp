#ifndef __SRC_HPP__
#define __SRC_HPP__
#include "tensor.hpp"

mlc::Tensor2<double> makeMatrix() {
    mlc::Tensor2<double> m({3, 4});
    auto mv = m.view();
    for (int i = 0; i < 3; i++)
        for (int j = 0; j < 4; j++)
            mv(i, j) = (double)(i * 4 + j + 1);
    return m;
}

// Uses the module type only inside this header: it appears in no morloc
// signature, so nothing from tensor-cpp is realized in the pool at all.
int cellCount() {
    mlc::Tensor2<double> m({2, 3});
    return (int)(m.view().extent(0) * m.view().extent(1));
}

double sumAll(const mlc::Tensor2<double>& m) {
    double s = 0;
    for (size_t k = 0; k < m.size(); k++) s += m.data()[k];
    return s;
}
#endif
