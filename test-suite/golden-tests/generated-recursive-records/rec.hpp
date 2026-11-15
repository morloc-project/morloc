#pragma once
#include "mlc/types/_local/main.hpp"
using namespace mlc::types::_local::main;

int sumChain(const Chain& c) { return c->val + (c->next ? sumChain(*c->next) : 0); }

int countTree(const Tree& t) {
  int n = 1;
  for (const auto& k : t->kids) n += countTree(k);
  return n;
}

int holderSum(const Holder& h) {
  int n = sumChain(h.one) + (h.maybe ? sumChain(*h.maybe) : 0);
  for (const auto& c : h.many) n += sumChain(c);
  return n + countTree(h.tree);
}

int deepBuild(int n) {
  std::optional<Chain> t;
  for (int i = 0; i < n; i++) t = Chain(Chain_fields{1, std::move(t)});
  int len = 0;
  for (const std::optional<Chain>* p = &t; *p; p = &(**p)->next) len++;
  return len;
}
