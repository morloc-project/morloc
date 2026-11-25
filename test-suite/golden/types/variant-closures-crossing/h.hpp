#pragma once
#include <functional>

struct Ops {
  std::function<int(int)> inc;
  std::function<int(int)> scale;
};

template <class T> T ident(T x) { return x; }

inline int add(int a, int b) { return a + b; }

inline int mul(int a, int b) { return a * b; }
