#pragma once
#include <functional>
#include <optional>
#include <tuple>
#include <vector>

struct Ops {
  std::function<int(int)> inc;
  std::function<int(int)> scale;
};

inline int useOps(Ops ops, int x) { return ops.inc(x) + ops.scale(x); }

inline int useList(std::vector<std::function<int(int)>> fs, int x) {
  int total = 0;
  for (auto& f : fs) total += f(x);
  return total;
}

inline int useTuple(std::tuple<std::function<int(int)>, std::function<int(int)>> t, int x) {
  return std::get<0>(t)(x) + std::get<1>(t)(x);
}

inline int useOpt(std::optional<std::function<int(int)>> f, int x) {
  return f.has_value() ? (*f)(x) : x;
}
