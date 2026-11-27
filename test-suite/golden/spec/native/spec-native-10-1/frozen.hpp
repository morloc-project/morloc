#pragma once
#include <vector>
template <class T>
std::vector<T> thaw(const std::vector<T>& x) { return x; }
inline std::vector<int> mk(int n) { return std::vector<int>(n, 1); }
