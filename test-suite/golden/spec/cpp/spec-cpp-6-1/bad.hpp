#ifndef __SPEC_CPP_6_1_BAD_HPP__
#define __SPEC_CPP_6_1_BAD_HPP__

#include <memory>
#include <variant>
#include <vector>

#include "mlc_rec.hpp"

inline std::vector<int> mk(int n) { return std::vector<int>(n, 1); }
inline int sz(const std::vector<int>& v) { return (int)v.size(); }

template <class B> B growArm(B b) { mlc::arm(std::get<0>(b.v))->f0.push_back(9); return b; }
template <class B> B growRaw(B b) { std::get<0>(b.v)->f0.push_back(9); return b; }
template <class B> B growVisit(B b) {
    std::visit([](auto& a) { a->f0.push_back(9); }, b.v);
    return b;
}
struct tl_t {
    int head;
    std::shared_ptr<const tl_t> tail;
};
inline tl_t bumpTail(tl_t l) { l.tail->head += 1; return l; }

#endif
