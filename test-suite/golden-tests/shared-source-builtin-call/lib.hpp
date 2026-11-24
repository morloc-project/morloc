#pragma once
#include <vector>

template <class T> T ident(T x) { return x; }

template <class T> int box_len(const std::vector<T>& xs) { return static_cast<int>(xs.size()); }
