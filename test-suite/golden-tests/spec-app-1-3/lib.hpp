#pragma once

#include <functional>

inline int make_adder(int n, int x) { return n + x; }

inline std::function<int(int)> make_adder1(int n) {
  return [n](int x) { return n + x; };
}
