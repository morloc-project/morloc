#pragma once
#include <vector>
inline std::vector<double> mkF(int k) { return std::vector<double>(k, 1.0); }
inline int lenF(const std::vector<double>& v) { return (int)v.size(); }
inline std::vector<double> mkT(int k) { return std::vector<double>(k, 2.0); }
inline int lenT(const std::vector<double>& v) { return (int)v.size(); }
inline std::vector<int> mkL(int k) { return std::vector<int>(k, 2); }
inline int sumL(const std::vector<int>& v) { int s = 0; for (int x : v) s += x; return s; }
