#pragma once
#include <vector>
template <class A> std::vector<A> revcpp(std::vector<A> xs){ return std::vector<A>(xs.rbegin(), xs.rend()); }
