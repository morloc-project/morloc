#pragma once
#include <cstdint>

inline int64_t note(int64_t x) { return x; }

template <class T>
T ident(const T& x) { return x; }

template <class F>
int64_t callOn(F f, int64_t x) { f(x); return x; }

template <class F, class A>
int64_t callWith(F f, const A& a) { return f(a); }
