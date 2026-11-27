#ifndef __SPEC_CPP_6_1_BAD_REC_HPP__
#define __SPEC_CPP_6_1_BAD_REC_HPP__

#include <memory>
#include <variant>
#include <vector>

inline std::vector<int> mk(int n) { return std::vector<int>(n, 1); }
struct node_t { int head; std::shared_ptr<node_t> tail; };
inline int nodeLen(const node_t& l) { return l.tail ? 2 : 1; }

#endif
