#pragma once
#include <memory>
struct ll_t { int head; std::shared_ptr<const ll_t> tail; };
inline ll_t mkLL(int n) {
    ll_t x{n, nullptr};
    for (int i = n - 1; i >= 1; i--) { x = ll_t{i, std::make_shared<ll_t>(x)}; }
    return x;
}
#include <vector>
struct node_t { int v; std::shared_ptr<const node_t> next; };
inline std::vector<node_t> mkNodes(int n) {
    std::vector<node_t> xs;
    for (int i = 0; i < n; i++) { xs.push_back(node_t{i, std::make_shared<node_t>(node_t{100 + i, nullptr})}); }
    return xs;
}
