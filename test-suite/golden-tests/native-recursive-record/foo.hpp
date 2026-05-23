#pragma once

#include <cstdint>
#include <memory>
#include <vector>

// Forward-declared recursive struct types.
// - tree_t: cycle broken by std::vector<tree_t> (vector's internal
//   indirection breaks the incomplete-type problem).
// - ll_t: cycle broken by std::shared_ptr<ll_t> at the optional
//   field. `nullptr == absent` matches the morloc-level ?LL
//   semantics directly (the morloc encoding rule maps `?(recursive
//   T)` to a single pointer-shape with null == absent).
struct tree_t;
struct ll_t;

// container_t<T>: parameterized recursive record.
// `sub` cycle broken by std::shared_ptr<container_t<T>>; nullptr == absent
// matches the morloc-level `?(Container a)` semantics.
template<typename T>
struct container_t {
    T val;
    std::shared_ptr<container_t<T>> sub;
};

struct tree_t {
    int64_t value;
    std::vector<tree_t> children;
};

struct ll_t {
    int64_t head;
    std::shared_ptr<ll_t> tail;
};

inline int64_t sum_tree(const tree_t& t) {
    int64_t total = t.value;
    for (const auto& c : t.children) {
        total += sum_tree(c);
    }
    return total;
}

inline int64_t ll_len(const ll_t& node) {
    int64_t n = 1;
    const ll_t* cur = &node;
    while (cur->tail) {
        n += 1;
        cur = cur->tail.get();
    }
    return n;
}

inline tree_t identity_tree(tree_t t) {
    return t;
}
