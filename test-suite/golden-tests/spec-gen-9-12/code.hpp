#pragma once
#include "mlc/types/_local/main.hpp"
inline int codeLen(const mlc::types::_local::main::Code& c) {
  int s = c.sym + (int)c.name.size() + c.pt.x + c.pt.y;
  for (int i : c.items) s += i;
  return s;
}
inline mlc::types::_local::main::Code mkCode(int n) {
  return {n, "ab", {n, n}, {1, 2}};
}
