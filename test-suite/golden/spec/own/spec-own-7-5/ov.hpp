#ifndef __SPEC_OWN_7_5_HPP__
#define __SPEC_OWN_7_5_HPP__

#include <functional>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

inline std::vector<int> mk(int n) {
    std::vector<int> v;
    for (int i = 1; i <= n; ++i) v.push_back(i);
    return v;
}

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

// The same with a leading option, for a partial application.
inline std::tuple<std::string, int> whichWith(int k, const std::vector<int>& xs) { return {"lv", k * total(xs)}; }
inline std::tuple<std::string, int> whichWith(int k, std::vector<int>&& xs) {
    std::vector<int> mine = std::move(xs);
    return {"rv", k * total(mine)};
}

// Hands f an owned batch it does not read again, as a stream stage does.
template <class F>
inline auto handOver(F f, int n) {
    auto b = mk(n);
    return f(std::move(b));
}

// Lends f a batch, then reads it again.
template <class F>
inline auto lendThenRead(F f, int n) {
    auto b = mk(n);
    auto r = f(b);
    return std::make_tuple(r, total(b));
}

// Gives f an owned batch through mlc::give, as a stream stage does.
template <class F>
inline auto giveTo(F f, int n) {
    auto b = mk(n);
#ifdef MLC_GIVE
    return mlc::give(f, std::move(b));
#else
    return f(std::move(b));
#endif
}

// Hands an rvalue to a std::function without mlc::give: it is lent.
inline std::tuple<std::string, int> viaStdFunction(const std::function<std::tuple<std::string, int>(const std::vector<int>&)>& f, int n) {
    return f(mk(n));
}

#endif
