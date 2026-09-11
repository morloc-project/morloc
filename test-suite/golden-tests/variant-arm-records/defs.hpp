#pragma once
#include <string>
#include <memory>

struct pt_t {
    std::string name;
    double x;
};

struct node_t {
    int v;
    std::shared_ptr<node_t> next;
};

template <typename T>
T ident(T x) {
    return x;
}
