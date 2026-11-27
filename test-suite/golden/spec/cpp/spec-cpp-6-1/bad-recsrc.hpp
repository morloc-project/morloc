#ifndef __SPEC_CPP_6_1_BAD_RECSRC_HPP__
#define __SPEC_CPP_6_1_BAD_RECSRC_HPP__

#include <memory>

struct snode_t { int head; std::shared_ptr<snode_t> tail; };
inline snode_t mkNode(int n) { return snode_t{n, std::make_shared<snode_t>(snode_t{n, nullptr})}; }
inline int nodeLen(const snode_t& l) { return l.tail ? 2 : 1; }

#endif
