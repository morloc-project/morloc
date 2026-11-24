#pragma once
#include <cstdint>
#include <cstdio>
#include <string>
struct date_t { int y; int m; int d; };
template <class A> struct pair_t { A fst; A snd; };
inline date_t packDate(const std::string& s) {
  date_t r{0, 0, 0};
  std::sscanf(s.c_str(), "%d-%d-%d", &r.y, &r.m, &r.d);
  return r;
}
inline std::string unpackDate(const date_t& x) {
  char b[32];
  std::snprintf(b, sizeof b, "%04d-%02d-%02d", x.y, x.m, x.d);
  return b;
}
inline double packTenths(int64_t x) { return double(x) / 10.0; }
inline int64_t unpackTenths(double x) { return int64_t(x * 10.0 + 0.5); }
template <class A> A same(const A& a) { return a; }
