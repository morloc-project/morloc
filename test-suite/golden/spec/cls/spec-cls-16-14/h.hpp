#pragma once
#include <map>
#include <string>
#include <tuple>
#include <vector>
template <class A>
std::map<std::string, A> h_pack(const std::vector<std::tuple<std::string, A>>& xs) {
  std::map<std::string, A> m;
  for (const auto& x : xs) m[std::get<0>(x)] = std::get<1>(x);
  return m;
}
template <class A>
std::vector<std::tuple<std::string, A>> h_unpack(const std::map<std::string, A>& m) {
  std::vector<std::tuple<std::string, A>> xs;
  for (const auto& kv : m) xs.push_back(std::make_tuple(kv.first, kv.second));
  return xs;
}
template <class A>
int h_size(const std::map<std::string, A>& m) { return static_cast<int>(m.size()); }
inline std::map<std::string, int> h_make(int n) {
  std::map<std::string, int> m;
  for (int i = 0; i < n; i++) m[std::to_string(i)] = i;
  return m;
}
