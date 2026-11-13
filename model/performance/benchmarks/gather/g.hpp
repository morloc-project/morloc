#pragma once
#include <cstdint>
#include <string>
#include <vector>

// k histograms of 256 counts each (2 KiB per element), varied by seed.
std::vector<std::vector<uint64_t>> mk_hists(int64_t k, int64_t seed) {
  std::vector<std::vector<uint64_t>> v(k, std::vector<uint64_t>(256));
  for (int64_t i = 0; i < k; i++)
    for (int j = 0; j < 256; j++) v[i][j] = (uint64_t)(i + j + seed);
  return v;
}

std::string summ(const std::vector<std::vector<uint64_t>>& xs) {
  uint64_t s = 0;
  for (auto& h : xs) s += h[7];
  return std::to_string(xs.size()) + " " + std::to_string(s);
}

int64_t count_hists(const std::vector<std::vector<uint64_t>>& xs) {
  return (int64_t)xs.size();
}
