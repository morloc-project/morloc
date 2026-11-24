#pragma once

template <class F> int run_cb(F f) { return f(1) + 1; }

inline int tick(int x) { return x + 40; }
