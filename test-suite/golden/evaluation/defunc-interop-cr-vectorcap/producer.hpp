#pragma once
#include <vector>
#include <numeric>

inline int tick() { return 0; }

inline int add(int a, int b) { return a + b; }

inline int vsum(std::vector<int> xs) { return std::accumulate(xs.begin(), xs.end(), 0); }
