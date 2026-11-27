#ifndef __SHM_RELEASE_STEADY_HPP__
#define __SHM_RELEASE_STEADY_HPP__

#include <vector>

inline std::vector<int> readingsCpp(int i) {
    std::vector<int> xs(3000);
    for (int j = 0; j < 3000; j++) xs[j] = i + j;
    return xs;
}

inline long long totalCpp(const std::vector<int>& xs) {
    long long s = 0;
    for (auto x : xs) s += x;
    return s;
}

#endif
