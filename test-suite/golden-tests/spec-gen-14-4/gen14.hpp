#pragma once
#include <cstdint>
#include <string>
#include "mlc/types/_local/main.hpp"
struct MlcDate { int64_t v; };
inline MlcDate packDate(const std::string& s) { return MlcDate{std::stoll(s)}; }
inline std::string unpackDate(const MlcDate& d) { return std::to_string(d.v); }
inline MlcDate dateOf(int x) { return MlcDate{int64_t(x) * 10}; }
template <class A> mlc::types::_local::main::Pair<A> dupP(const A& x) { return {x, x}; }
inline int firstDay(const mlc::types::_local::main::Pair<MlcDate>& p) { return int(p.fst.v); }
template <class A> mlc::types::_local::main::Bin<A> boxOf(const A& x) { return {x, 2}; }
