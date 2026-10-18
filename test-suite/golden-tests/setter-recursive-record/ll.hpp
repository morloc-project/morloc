#ifndef LL_HPP
#define LL_HPP
#include <memory>
#include <vector>
struct ll_t;
struct ll_t {
    int head;
    std::shared_ptr<ll_t> tail;
};
inline int bump(int x) { return x + 1; }
inline int depth(const ll_t& l) { int d = 1; const ll_t* p = &l; while (p->tail) { p = p->tail.get(); d++; } return d; }
#endif
