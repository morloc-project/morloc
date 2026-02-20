#ifndef MORLOC_STRESS_HPP
#define MORLOC_STRESS_HPP

#include <vector>

int sum_list(const std::vector<int>& xs) {
    int s = 0;
    for (auto x : xs) s += x;
    return s;
}

#endif
