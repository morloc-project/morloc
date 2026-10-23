#ifndef __SPEC_CPP_1_1_HPP__
#define __SPEC_CPP_1_1_HPP__

#include <cstdint>
#include <functional>
#include <vector>

inline std::vector<int> mk(int n) { return std::vector<int>(n, 1); }

inline int bumpRef(std::vector<int>& v) { v.push_back(1); return (int)v.size(); }
inline int bumpConst(const std::vector<int>& v) { return (int)v.size() + 1; }
inline int sizeV(std::vector<int> v) { return (int)v.size(); }

template <class T> int fwdSize(T&& v) { return (int)v.size(); }

inline int ovl(const std::vector<int>& v) { return (int)v.size(); }
inline int ovl(std::vector<int>&) { return -1; }

inline int addRef(int& acc, int x) { acc += x; return acc; }
inline int addConst(const int& acc, int x) { return acc + x; }

inline bool ltRef(int& a, int& b) { return a < b; }
inline bool ltConst(const int& a, const int& b) { return a < b; }

inline int pick(bool b, int x) { return b ? x : 0; }
inline int applyFn(std::function<int(int)> f, int x) { return f(x); }
inline int applyFnRef(std::function<int(int)>& f, int x) {
    int r = f(x);
    f = [](int) { return 100; };
    return r;
}
template <class F> int applyFnConst(const F& f, int x) { return f(x); }
inline int inc(int x) { return x + 1; }

enum class Color : uint8_t { Red = 0, Green = 1 };
inline int colorCode(Color c) { return c == Color::Red ? 1 : 2; }

inline int operator+(std::vector<int>& a, const std::vector<int>& b) {
    a.push_back(1);
    return (int)(a.size() + b.size());
}
inline int operator-(const std::vector<int>& a, const std::vector<int>& b) {
    return (int)(a.size() - b.size());
}

inline int operator*(int& s, const std::vector<int>& v) {
    s += 1;
    return s * (int)v.size();
}
inline int operator/(const int& s, const std::vector<int>& v) {
    return s * (int)v.size();
}

#endif
