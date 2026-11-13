#pragma once
#include <cstdint>
#include <string>
#include <tuple>
#include <vector>

std::vector<double> gen_reals(int64_t n) {
  std::vector<double> v(n);
  for (int64_t i = 0; i < n; i++) v[i] = (i + 1) * 0.5;
  return v;
}

std::vector<std::string> gen_strs(int64_t n) {
  std::vector<std::string> v(n);
  for (int64_t i = 0; i < n; i++) v[i] = std::string(8 + i % 17, "ACGT"[i % 4]);
  return v;
}

std::vector<std::tuple<int64_t, double, double>> gen_tups(int64_t n) {
  std::vector<std::tuple<int64_t, double, double>> v(n);
  for (int64_t i = 0; i < n; i++) v[i] = {i, i * 0.25, i * 1.5};
  return v;
}
