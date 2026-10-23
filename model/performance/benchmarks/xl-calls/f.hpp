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
