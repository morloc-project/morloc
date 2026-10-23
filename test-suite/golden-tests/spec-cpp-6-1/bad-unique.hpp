#ifndef __SPEC_CPP_6_1_BAD_UNIQUE_HPP__
#define __SPEC_CPP_6_1_BAD_UNIQUE_HPP__

#include <memory>
#include <variant>
#include <vector>

inline std::vector<int> mk(int n) { return std::vector<int>(n, 1); }
struct unode_t { int head; std::unique_ptr<const unode_t> tail; };
inline int unodeLen(const unode_t& l) { return l.tail ? 2 : 1; }

#endif
