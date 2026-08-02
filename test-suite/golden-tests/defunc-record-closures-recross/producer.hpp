#pragma once
#include <functional>
#include <vector>
inline int tickCpp() { return 0; }
inline int addCpp(int a, int b) { return a + b; }
inline int applyCpp(std::vector<std::function<int(int)>> fs, int x) { return fs[0](x); }
