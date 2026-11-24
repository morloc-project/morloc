#pragma once
#include <vector>
struct tree_t { double t; std::vector<tree_t> kids; };
inline tree_t mk(int n) {
  tree_t x{n / 10.0, {}};
  if (n > 0) x.kids.push_back(mk(n - 1));
  return x;
}
inline void tensGo(const tree_t& x, std::vector<int>& out) {
  out.push_back(int(x.t * 10.0 + 0.5));
  for (const auto& k : x.kids) tensGo(k, out);
}
inline std::vector<int> tens(const tree_t& x) { std::vector<int> o; tensGo(x, o); return o; }
