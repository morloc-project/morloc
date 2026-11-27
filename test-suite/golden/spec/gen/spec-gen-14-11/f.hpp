#pragma once
#include <vector>
#include "mlc/types/_local/main.hpp"
using mlc::types::_local::main::Tree;
using mlc::types::_local::main::Tree_fields;
inline Tree mk(int n) {
  Tree_fields f;
  f.t = n / 10.0;
  if (n > 0) f.kids.push_back(mk(n - 1));
  return Tree(std::move(f));
}
inline void tensGo(const Tree& x, std::vector<int>& out) {
  out.push_back(int(x->t * 10.0 + 0.5));
  for (const auto& k : x->kids) tensGo(k, out);
}
inline std::vector<int> tens(const Tree& x) { std::vector<int> o; tensGo(x, o); return o; }
