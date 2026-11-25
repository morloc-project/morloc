#ifndef __PHANTOM_NAT_OPS_HPP__
#define __PHANTOM_NAT_OPS_HPP__

#include <vector>

double sumVec_cpp(const std::vector<double>& xs) {
    double s = 0.0;
    for (double x : xs) s += x;
    return s;
}

std::vector<double> makeVec_cpp() {
    return std::vector<double>{100.0, 200.0, 300.0, 400.0};
}

#endif
