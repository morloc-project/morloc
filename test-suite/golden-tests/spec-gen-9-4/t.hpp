#pragma once
#include "mlc/types/_local/main.hpp"
template <class A> A same(const A& x) { return x; }
inline bool isPicked(const mlc::types::_local::main::R<int>& r) {
  const mlc::types::_local::main::Choice<int>& c = r.d;
  return c.v.index() == 0;
}
#include <string>
struct MlcDate { long v; };
inline MlcDate packDate(const std::string& s) { return MlcDate{std::stol(s)}; }
inline std::string unpackDate(const MlcDate& d) { return std::to_string(d.v); }
inline bool outerPicked(const mlc::types::_local::main::Choice<mlc::types::_local::main::Choice<int>>& c) { return c.v.index() == 0; }
template <class T> int one(const T&) { return 1; }
