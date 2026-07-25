#pragma once
#include <functional>
inline int tick() { return 0; }
inline int mul(int a, int b) { return a * b; }
inline int add3(int a, int b, int c) { return a + b + c; }
inline int applyIt(std::function<int(int)> f, int x) { return f(x); }
inline int applyTwice(std::function<int(int)> f, int x) { return f(f(x)); }
