#ifndef __SHM_RELEASE_UAF_HPP__
#define __SHM_RELEASE_UAF_HPP__

#include <functional>
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

inline long long pairTotalCpp(const std::vector<int>& a, const std::vector<int>& b) {
    return totalCpp(a) * 7 + totalCpp(b);
}

inline long long applyNCpp(std::function<long long(int)> f, int n) {
    long long s = 0;
    for (int i = 0; i < n; i++) s += f(i);
    return s;
}

inline std::vector<int> idCpp(const std::vector<int>& xs) {
    return xs;
}

#endif
