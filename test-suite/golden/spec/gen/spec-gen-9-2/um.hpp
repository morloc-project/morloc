#pragma once
#include <cstdint>
template <class A> struct pair_t { A fst; A snd; };
inline double packTenths(int64_t x) { return double(x) / 10.0; }
inline int64_t unpackTenths(double x) { return int64_t(x * 10.0); }
inline double tenthsOf(int64_t x) { return double(x) / 10.0; }
template <class A> pair_t<A> dupU(const A& x) { return {x, x}; }
inline int64_t sumP(const pair_t<double>& p) { return int64_t((p.fst + p.snd) * 10.0 + 0.5); }
