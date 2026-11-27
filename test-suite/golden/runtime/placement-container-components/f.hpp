#include <tuple>

int cpp_b(int x){ return x + 2; }
int cpp_c(int x){ return x + 3; }
int consume(std::tuple<int,int,int> t){ return std::get<0>(t) + std::get<1>(t) + std::get<2>(t); }
