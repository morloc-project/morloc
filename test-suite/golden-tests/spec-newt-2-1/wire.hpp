#pragma once
#include <string>
#include <tuple>

inline int str_len(const std::string& s){ return (int)s.size(); }

inline int pair_sum(const std::tuple<int,int>& p){
    return std::get<0>(p) + std::get<1>(p);
}
