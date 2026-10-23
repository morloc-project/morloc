#pragma once
#include <cstdint>
#include <string>
#include <vector>

std::vector<double> mk_reals(int64_t n) {
  std::vector<double> v(n);
  for (int64_t i = 0; i < n; i++) v[i] = (double)i;
  return v;
}

int64_t len_reals(const std::vector<double>& xs) { return xs.size(); }

std::string mk_str(int64_t n) { return std::string(n, 'x'); }

int64_t len_str(const std::string& s) { return s.size(); }

std::vector<std::string> mk_strs(int64_t n) { return std::vector<std::string>(n, "abcdefgh"); }

int64_t len_strs(const std::vector<std::string>& xs) { return xs.size(); }
