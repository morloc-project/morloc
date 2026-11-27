#pragma once
#include <cstdint>
#include <optional>
#include <tuple>
#include <vector>
template <class A> struct pair_t { A fst; A snd; };
struct point_t { double fst; double snd; };
template <class A> struct w_t { std::tuple<A,A> fst; std::tuple<A,A> snd; };
inline double packTenths(int64_t x) { return double(x) / 10.0; }
inline int64_t unpackTenths(double x) { return int64_t(x * 10.0 + 0.5); }
inline int64_t readPt(const point_t& p) { return int64_t(p.fst * 100.0 + 0.5) + int64_t(p.snd * 10.0 + 0.5); }
inline point_t mkPt(int64_t n) { return {double(n) / 10.0, double(n + 1) / 10.0}; }
inline int64_t readPts(const std::vector<point_t>& ps) { int64_t s = 0; for (auto& p : ps) s += readPt(p); return s; }
inline int64_t readOpt(const std::optional<point_t>& p) { return p ? readPt(*p) : -1; }
inline int64_t readW(const w_t<double>& p) { return int64_t(std::get<0>(p.fst) * 1000.0 + 0.5) + int64_t(std::get<1>(p.snd) * 10.0 + 0.5); }
