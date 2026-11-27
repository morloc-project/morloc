#pragma once
#include <cstdio>
#include <tuple>
#include "mlccpptypes/prelude.hpp"

inline int mk(int i) { return i * 10; }
inline int tr(int c) { return c + 100; }
inline std::tuple<int,int> pairUp(int c) { return std::make_tuple(c + 100, c * 2); }
inline mlc::Unit emit(int c) { printf("%d\n", c); return mlc::Unit{}; }
