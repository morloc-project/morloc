#pragma once
#include <cstdint>
#include <functional>
#include <map>
#include <string>
#include <vector>

// b batches of s keys in [0, nkeys).
inline void produce(int64_t nkeys, int64_t s, int64_t b, std::function<void(std::vector<int64_t>)> sink) {
  uint64_t x = 12345;
  for (int64_t i = 0; i < b; i++) {
    std::vector<int64_t> v(s);
    for (int64_t j = 0; j < s; j++) { x = x * 6364136223846793005ULL + 1442695040888963407ULL; v[j] = (int64_t)((x >> 33) % (uint64_t)nkeys); }
    sink(std::move(v));
  }
}

inline int64_t zero_n() { return 0; }
inline int64_t add_len(int64_t acc, const std::vector<int64_t>& v) { return acc + (int64_t)v.size(); }
inline int64_t plus(int64_t a, int64_t b) { return a + b; }
inline std::string show_n(int64_t n) { return std::to_string(n) + "\n"; }

inline std::vector<int64_t> zero_hist() { return std::vector<int64_t>(256, 0); }
inline std::vector<int64_t> add_hist(const std::vector<int64_t>& h, const std::vector<int64_t>& v) {
  std::vector<int64_t> r = h;
  for (auto k : v) r[k & 255]++;
  return r;
}
inline std::vector<int64_t> plus_hist(const std::vector<int64_t>& a, const std::vector<int64_t>& b) {
  std::vector<int64_t> r = a;
  for (size_t i = 0; i < r.size() && i < b.size(); i++) r[i] += b[i];
  return r;
}
inline std::string show_hist(const std::vector<int64_t>& h) {
  int64_t s = 0;
  for (auto c : h) s += c;
  return std::to_string(s) + "\n";
}

inline std::map<int64_t, int64_t> zero_map() { return {}; }
inline std::map<int64_t, int64_t> add_map(const std::map<int64_t, int64_t>& m, const std::vector<int64_t>& v) {
  std::map<int64_t, int64_t> r = m;
  for (auto k : v) r[k]++;
  return r;
}
inline std::map<int64_t, int64_t> plus_map(const std::map<int64_t, int64_t>& a, const std::map<int64_t, int64_t>& b) {
  std::map<int64_t, int64_t> r = a;
  for (auto& kv : b) r[kv.first] += kv.second;
  return r;
}
inline std::string show_map(const std::map<int64_t, int64_t>& m) {
  int64_t s = 0;
  for (auto& kv : m) s += kv.second;
  return std::to_string(m.size()) + " " + std::to_string(s) + "\n";
}

// The same work done where the batches are made: no sink, no fold.
inline std::string control_map(int64_t nkeys, int64_t s, int64_t b) {
  std::map<int64_t, int64_t> m;
  produce(nkeys, s, b, [&](std::vector<int64_t> v) { m = add_map(m, v); });
  return show_map(m);
}
inline std::string control_n(int64_t nkeys, int64_t s, int64_t b) {
  int64_t n = 0;
  produce(nkeys, s, b, [&](std::vector<int64_t> v) { n = add_len(n, v); });
  return show_n(n);
}
