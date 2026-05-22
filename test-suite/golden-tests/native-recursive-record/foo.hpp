#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <vector>

// Forward-declared recursive struct types. Recursive fields must use
// either std::vector<T> (size-bounded by base case []) or
// std::optional<T> (size-bounded by base case nullopt). Direct
// std::optional<T> of an incomplete T is not legal in C++; wrap via
// pointer for the LL case.
struct tree_t;
struct ll_t;

struct tree_t {
    int64_t value;
    std::vector<tree_t> children;
};

struct ll_t {
    int64_t head;
    std::optional<std::unique_ptr<ll_t>> tail;
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
    while (cur->tail.has_value()) {
        n += 1;
        cur = cur->tail->get();
    }
    return n;
}

inline tree_t identity_tree(tree_t t) {
    return t;
}
