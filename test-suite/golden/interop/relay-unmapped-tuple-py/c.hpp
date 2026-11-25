#pragma once
#include <vector>
#include <tuple>
#include <cstdint>
inline std::vector<std::tuple<int,int,int,int>> mk4(int n){ return {std::make_tuple(n,n,n,n)}; }
inline int sum4(const std::vector<std::tuple<int,int,int,int>>& xs){ int s=0; for(auto& t: xs) s += std::get<0>(t)+std::get<3>(t); return s; }
inline int cppio(int n){ return n; }
