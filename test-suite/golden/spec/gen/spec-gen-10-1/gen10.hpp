#pragma once
struct Point { int x; int y; };
inline Point mkPoint(int n) { return Point{n, n + 1}; }
template <class T> T idn(const T& x) { return x; }
