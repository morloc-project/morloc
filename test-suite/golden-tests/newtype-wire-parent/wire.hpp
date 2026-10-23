#pragma once
#include <string>
#include <tuple>
#include <vector>

inline int str_len(const std::string& s){ return (int)s.size(); }

inline int sum_ints(const std::vector<int>& xs){
    int s = 0;
    for (int x : xs) s += x;
    return s;
}

inline int pair_sum(const std::tuple<int,int>& p){
    return std::get<0>(p) + std::get<1>(p);
}
