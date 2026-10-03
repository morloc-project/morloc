#ifndef __APPLY_HPP__
#define __APPLY_HPP__
#include <functional>

inline int cppApply(std::function<int(int)> f, int x) { return f(x); }
#endif
