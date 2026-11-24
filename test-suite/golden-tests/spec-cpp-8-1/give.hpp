#ifndef __SPEC_CPP_8_1_HPP__
#define __SPEC_CPP_8_1_HPP__

#include <string>
#include <tuple>
#include <utility>
#include <vector>

inline int total(const std::vector<int>& xs) {
    int s = 0;
    for (int x : xs) s += x;
    return s;
}

// Which overload ran, and the sum of what it was given.
inline std::tuple<std::string, int> which(const std::vector<int>& xs) { return {"lv", total(xs)}; }
inline std::tuple<std::string, int> which(std::vector<int>&& xs) {
    std::vector<int> mine = std::move(xs);
    return {"rv", total(mine)};
}

// Gives f an owned list through the user API when it is available.
template <class F>
inline auto giveTo(F f, int n) {
    std::vector<int> b;
    for (int i = 1; i <= n; ++i) b.push_back(i);
#ifdef MLC_GIVE
    return mlc::give(f, std::move(b));
#else
    return f(std::move(b));
#endif
}

#endif
