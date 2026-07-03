#ifndef __SRC_HPP__
#define __SRC_HPP__
#include "tensor.hpp"

mlc::Tensor2<int> makeMatrix() {
    mlc::Tensor2<int> m({3, 4});
    auto mv = m.view();
    for (int i = 0; i < 3; i++)
        for (int j = 0; j < 4; j++)
            mv(i, j) = i * 4 + j + 1;
    return m;
}
#endif
