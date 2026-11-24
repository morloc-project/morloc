#pragma once
#include <vector>
template <class T> struct foo_t { T x; };
inline foo_t<int> mkFoo(int k) { return foo_t<int>{k}; }
inline int getX(const foo_t<int>& f) { return f.x; }
struct bar_t { foo_t<int> f; };
inline bar_t mkBar(int k) { return bar_t{foo_t<int>{k}}; }
inline int getBX(const bar_t& b) { return b.f.x; }
inline std::vector<int> mkH(int k) { return std::vector<int>{k, k}; }
struct baz_t { std::vector<int> h; };
inline baz_t mkBaz(int k) { return baz_t{std::vector<int>{k}}; }
