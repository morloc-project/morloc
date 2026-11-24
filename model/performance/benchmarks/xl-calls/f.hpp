#pragma once
#include <cstdint>

int64_t inc(int64_t x) { return x + 1; }

template <class F>
int64_t mapf(F f, int64_t n) {
  int64_t s = 0;
  for (int64_t i = 0; i < n; i++) s += f(i);
  return s;
}

int64_t native(int64_t n) {
  int64_t s = 0;
  for (int64_t i = 0; i < n; i++) s += inc(i);
  return s;
}

#include <vector>

struct Rec {
  int64_t ra; double rb; int64_t rc; double rd;
  int64_t re; double rf; int64_t rg; double rh;
};

int64_t touch(const Rec& r) { return r.ra + r.rc; }

template <class F>
int64_t maprec(F f, int64_t n) {
  int64_t s = 0;
  for (int64_t i = 0; i < n; i++) s += f(Rec{i, 1.0, 2, 3.0, 4, 5.0, 6, 7.0});
  return s;
}

double vsum(const std::vector<double>& xs) {
  double s = 0;
  for (double x : xs) s += x;
  return s;
}

template <class F>
double mapvec(F f, int64_t m, int64_t n) {
  std::vector<double> xs(m);
  for (int64_t i = 0; i < m; i++) xs[i] = (double)i;
  double s = 0;
  for (int64_t i = 0; i < n; i++) s += f(xs);
  return s;
}
