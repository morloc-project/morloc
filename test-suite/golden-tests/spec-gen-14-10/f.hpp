#pragma once
#include "mlc/types/_local/main.hpp"
inline mlc::types::_local::main::B mkB(int k) {
  mlc::types::_local::main::B b;
  b.t = k / 10.0;
  b.n = k;
  return b;
}
