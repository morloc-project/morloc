#pragma once
#include <cstdint>
#include <vector>

std::vector<double> realsC(int64_t n) {
  std::vector<double> v(n);
  for (int64_t i = 0; i < n; i++) v[i] = i * 1.000123 + 0.5;
  return v;
}

int64_t countC(const std::vector<double>& v) { return (int64_t)v.size(); }
