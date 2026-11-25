#pragma once
#include <functional>

inline int applyIt(std::function<int(int)> f, int x) { return f(x); }

inline int applyIt2(std::function<int(int,int)> f, int x, int y) { return f(x, y); }
