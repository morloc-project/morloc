#pragma once
#include <vector>

double total(const std::vector<double>& xs) {
    double s = 0;
    for (double x : xs) s += x;
    return s;
}
