#ifndef __SRC_HPP__
#define __SRC_HPP__
#include "mlc_tensor.hpp"

mlc::Tensor2<double> makeMatrix() {
    mlc::Tensor2<double> m({3, 4});
    auto mv = m.view();
    for (int i = 0; i < 3; i++)
        for (int j = 0; j < 4; j++)
            mv(i, j) = (double)(i * 4 + j + 1);
    return m;
}
#endif
