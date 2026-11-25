#pragma once
#include <functional>

inline int use_pure(std::function<int(int)> f, int x) { return f(x); }
