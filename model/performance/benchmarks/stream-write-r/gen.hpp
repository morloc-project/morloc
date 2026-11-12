#pragma once
#include <cstdint>
#include <vector>

std::vector<double> gen_reals(int64_t n) {
  std::vector<double> v(n);
  for (int64_t i = 0; i < n; i++) v[i] = (i + 1) * 0.5;
  return v;
}

std::vector<int32_t> gen_ints(int64_t n) {
  std::vector<int32_t> v(n);
  for (int64_t i = 0; i < n; i++) v[i] = (int32_t)((i + 1) % 1000);
  return v;
}

std::vector<bool> gen_bools(int64_t n) {
  std::vector<bool> v(n);
  for (int64_t i = 0; i < n; i++) v[i] = (i + 1) % 3 == 0;
  return v;
}

template <class T> int64_t len(const std::vector<T>& xs) { return (int64_t)xs.size(); }
