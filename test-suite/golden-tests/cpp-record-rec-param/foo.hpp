#pragma once
template <class T> struct foo_t { T x; };
inline foo_t<int> mkFoo(int k) { return foo_t<int>{k}; }
inline int getX(const foo_t<int>& f) { return f.x; }
