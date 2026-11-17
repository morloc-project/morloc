#pragma once
#include <vector>
struct tree_t { int val; std::vector<tree_t> kids; };
struct forest_t { int val; std::vector<tree_t> kids; };
template <class T> T same(T x) { return x; }
