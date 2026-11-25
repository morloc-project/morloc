#ifndef __SPEC_CPP_6_1_HPP__
#define __SPEC_CPP_6_1_HPP__

#include <memory>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

inline std::vector<int> mk(int n) { return std::vector<int>(n, 1); }
inline int sz(const std::vector<int>& v) { return (int)v.size(); }

// A generated `data` value changed by copying its arm and rebuilding it.
template <class B> B growGen(B b) {
    using Arm = std::remove_const_t<typename std::variant_alternative_t<0, decltype(b.v)>::element_type>;
    Arm a = *std::get<0>(b.v);
    a.f0.push_back(9);
    b.v = std::make_shared<Arm>(std::move(a));
    return b;
}

struct Shape_Circle;
struct Shape_Dot;
struct Shape {
    std::variant<std::shared_ptr<const Shape_Circle>, std::shared_ptr<const Shape_Dot>> v;
};
struct Shape_Circle { std::vector<int> f0; };
struct Shape_Dot { };

inline Shape grow(Shape s) {
    if (auto p = std::get_if<std::shared_ptr<const Shape_Circle>>(&s.v)) {
        Shape_Circle a = **p;
        a.f0.push_back(9);
        s.v = std::make_shared<Shape_Circle>(std::move(a));
    }
    return s;
}

inline int area(const Shape& s) {
    if (auto p = std::get_if<std::shared_ptr<const Shape_Circle>>(&s.v)) return sz((*p)->f0);
    return 0;
}

struct ll_t {
    int head;
    std::shared_ptr<const ll_t> tail;
};

inline int lenL(const ll_t& l) {
    int d = 1;
    for (const ll_t* p = &l; p->tail; p = p->tail.get()) d++;
    return d;
}

inline ll_t prepend(int x, ll_t l) {
    return ll_t{x, std::make_shared<ll_t>(std::move(l))};
}

// Built front to back through non-const handles, then stored as const.
inline ll_t buildList(int n) {
    ll_t first{0, nullptr};
    std::shared_ptr<ll_t> last;
    for (int i = 1; i < n; i++) {
        auto node = std::make_shared<ll_t>(ll_t{i, nullptr});
        if (last) last->tail = node; else first.tail = node;
        last = node;
    }
    return first;
}

#endif
