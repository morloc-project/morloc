#pragma once
#include <functional>
#include <vector>

inline int applyIt(std::function<int(int)> f, int x) { return f(x); }

inline int applyIt2(std::function<int(int,int)> f, int x, int y) { return f(x, y); }

inline std::vector<int> applyEach(std::vector<std::function<int(int)>> fs, int x) {
  std::vector<int> out;
  for (auto& f : fs) out.push_back(f(x));
  return out;
}
