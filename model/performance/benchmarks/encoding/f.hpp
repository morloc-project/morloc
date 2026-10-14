#pragma once
#include <cstdint>
#include <string>
#include <vector>

std::vector<int> mk_ints(int64_t n) {
  std::vector<int> v(n);
  for (int64_t i = 0; i < n; i++) v[i] = (int)i;
  return v;
}

std::vector<uint64_t> mk_u64s(int64_t n) {
  std::vector<uint64_t> v(n);
  for (int64_t i = 0; i < n; i++) v[i] = (uint64_t)i;
  return v;
}

std::vector<double> mk_reals(int64_t n) {
  std::vector<double> v(n);
  for (int64_t i = 0; i < n; i++) v[i] = i * 0.5;
  return v;
}

std::vector<bool> mk_bools(int64_t n) {
  std::vector<bool> v(n);
  for (int64_t i = 0; i < n; i++) v[i] = i % 2;
  return v;
}

std::vector<std::string> mk_strs(int64_t n) { return std::vector<std::string>(n, "abcdefgh"); }

template <class T>
int64_t len(const std::vector<T>& xs) { return (int64_t)xs.size(); }
