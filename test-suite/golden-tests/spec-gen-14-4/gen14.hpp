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
#include <vector>
inline int sumDays(const std::vector<mlc::types::_local::main::Pair<MlcDate>>& xs) { int s = 0; for (auto& p : xs) s += int(p.fst.v); return s; }
template <class F> void producePairs(F sink) { for (int i = 1; i <= 2; i++) { std::vector<mlc::types::_local::main::Pair<MlcDate>> b; for (int j = 0; j < i; j++) b.push_back(dupP(dateOf(i * 10 + j))); sink(b); } }
inline int addPairs(int acc, const std::vector<mlc::types::_local::main::Pair<MlcDate>>& xs) { return acc + sumDays(xs); }
inline int mergeSums(int a, int b) { return a + b; }
inline std::string showSum(int n) { return "sum=" + std::to_string(n) + "\n"; }
