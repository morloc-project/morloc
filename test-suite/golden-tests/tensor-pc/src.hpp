#ifndef __SRC_HPP__
#define __SRC_HPP__
#include "mlc_tensor.hpp"

double sumAll(const mlc::Tensor2<double>& m) {
    double s = 0;
    for (size_t k = 0; k < m.size(); k++) s += m.data()[k];
    return s;
}
#endif
