#pragma once
#include <vector>
#include <string>

inline std::vector<int> mk(int i){ return {i * 10, i * 10 + 1}; }

// render / render.buffer handler: [Int] -> Str (emitted verbatim).
inline std::string joinAll(std::vector<int> xs){
  std::string s;
  for (size_t i = 0; i < xs.size(); ++i){ if (i) s += ","; s += std::to_string(xs[i]); }
  s += "\n";
  return s;
}

// with.buffer handler: [Int] -> [Int] (per-batch map; nexus formats the result).
inline std::vector<int> dbl(std::vector<int> xs){
  std::vector<int> r;
  for (int x : xs) r.push_back(x * 2);
  return r;
}

// whole-form with handler: [Int] -> Int (gather-then-apply; nexus formats it).
inline int total(std::vector<int> xs){
  int s = 0;
  for (int x : xs) s += x;
  return s;
}
