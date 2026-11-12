#pragma once
#include <string>
#include <vector>

std::vector<int> mk(int n) { return std::vector<int>{n}; }

std::string body(int n) { return std::string((size_t)n, 'a'); }

std::string render(const std::vector<int>& xs) {
  return xs.empty() ? std::string() : body(xs[0]);
}

int gen_only(int k, int n) {
  int total = 0;
  for (int i = 0; i < k; i++) total += (int)(body(n).size() > 0);
  return total;
}
