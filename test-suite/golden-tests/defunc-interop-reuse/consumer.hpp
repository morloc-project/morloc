#pragma once

template <class F>
inline int applyTwice(F f, int a, int b) { return f(a) + f(b); }
