#pragma once
#include <cstdint>
#include "mlc/types/_local/main.hpp"
template <class A> struct u_t { A x; mlc::types::_local::main::G2<A> y; };
inline double packTenths(int64_t x) { return double(x) / 10.0; }
inline int64_t unpackTenths(double x) { return int64_t(x * 10.0 + 0.5); }
inline u_t<double> mkU(int64_t n) { return {double(n) / 10.0, {double(n + 1) / 10.0}}; }
inline int64_t readU(const u_t<double>& u) { return int64_t(u.x * 100.0 + 0.5) + int64_t(u.y.z * 10.0 + 0.5); }
template <class A> A same(const A& a) { return a; }
inline u_t<double> bumpU(const u_t<double>& u) { return {u.x + 0.1, {u.y.z + 0.2}}; }
