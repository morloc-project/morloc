#pragma once
#include <vector>
#include <optional>
#include <cstdint>
template <class T> T h_id(const T& x){ return x; }
template <class T> T h_head(const std::vector<T>& x){ return x[0] + 1000; }
template <class T> T h_general(std::optional<int64_t> i, const std::vector<T>& x){ return x[*i] + 100; }
inline int h_inc(int n){ return n+1; }
