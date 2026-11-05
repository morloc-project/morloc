#pragma once
#include <vector>

struct lbl_t { int v; };

inline lbl_t pack_lbl(int x) { return lbl_t{x}; }
inline int unpack_lbl(const lbl_t& x) { return x.v; }

struct tree_t {
  lbl_t label;
  std::vector<tree_t> children;
};

inline tree_t mk_tree(int k) {
  if (k <= 0) return tree_t{lbl_t{1}, {}};
  return tree_t{lbl_t{k}, {mk_tree(k - 1), mk_tree(k - 1)}};
}

inline int sum_tree(const tree_t& t) {
  int s = t.label.v;
  for (const auto& c : t.children) s += sum_tree(c);
  return s;
}
