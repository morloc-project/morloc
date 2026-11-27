#pragma once
#include "mlc/types/_local/main.hpp"
template <class A> A same(const A& x) { return x; }
inline int depthOf(const mlc::types::_local::main::Container<int>& c) {
  int d = 0;
  mlc::types::_local::main::Container<int> cur = c;
  while (cur->sub) { d++; cur = *cur->sub; }
  return d;
}
