#pragma once
#include <cstdint>
#include <string>
#include <vector>
#include "mlc/types/_local/main.hpp"

using mlc::types::_local::main::Pair;

inline int64_t packDate(const std::string& s) { return std::stoll(s); }
inline std::string unpackDate(int64_t d) { return std::to_string(d); }
inline int64_t dateOf(int x) { return int64_t(x) * 10; }

template <class A> Pair<A> dupP(const A& x) { return {x, x}; }
template <class A> Pair<A> swapP(const Pair<A>& p) { return {p.snd, p.fst}; }
template <class A> A firstP(const Pair<A>& p) { return p.fst; }

inline std::vector<int> ints(int n) { std::vector<int> v(n); for (int i = 0; i < n; i++) v[i] = i; return v; }
inline int total(const Pair<std::vector<int>>& p) { long s = 0; for (int x : p.fst) s += x; for (int x : p.snd) s += x; return int(s % 1000003); }
