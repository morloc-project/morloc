#pragma once
#include "mlc/types/_local/main.hpp"

int bump(int x) { return x + 1; }

int applyOps(const mlc::types::_local::main::Ops& o, int x) { return o.step(x) * 10; }

mlc::types::_local::main::Ops makeOps(const std::string& n) {
  return mlc::types::_local::main::Ops{[](int x) { return x * 2; }, n};
}

int applyChain(const mlc::types::_local::main::Chain& c, int x) {
  int y = c->run(x);
  if (c->next) return applyChain(*c->next, y);
  return y;
}
