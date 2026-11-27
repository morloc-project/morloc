#pragma once
#include <vector>
template <class T, class Sink>
inline void emit(std::vector<T> xs, Sink write_out) { write_out(std::move(xs)); }
