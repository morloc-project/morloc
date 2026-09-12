#pragma once
#include <algorithm>
#include <memory>
#include <string>
#include <variant>
#include <vector>

// A user-mapped `data` with a parameter is a class template. Its arms are
// templates too, named after the wrapper with `_<Constructor>` appended to
// the head and taking the same arguments: `MyBox<$1>` has arms
// `MyBox_Empty<$1>` and `MyBox_Full<$1>`.
template <typename T> struct MyBox_Empty;
template <typename T> struct MyBox_Full;

template <typename T> struct MyBox {
    std::variant<std::shared_ptr<MyBox_Empty<T>>,
                 std::shared_ptr<MyBox_Full<T>>> v;
};

template <typename T> struct MyBox_Empty {};
template <typename T> struct MyBox_Full { T f0; };

template <typename T> struct MyWrap { T item; };

template <typename T> struct MyTree_Leaf;
template <typename T> struct MyTree_Node;

template <typename T> struct MyTree {
    std::variant<std::shared_ptr<MyTree_Leaf<T>>,
                 std::shared_ptr<MyTree_Node<T>>> v;
};

template <typename T> struct MyTree_Leaf { T f0; };
template <typename T> struct MyTree_Node { MyTree<T> f0; MyTree<T> f1; };

template <typename T> struct MyRose_Tip;
template <typename T> struct MyRose_Branch;

template <typename T> struct MyRose {
    std::variant<std::shared_ptr<MyRose_Tip<T>>,
                 std::shared_ptr<MyRose_Branch<T>>> v;
};

template <typename T> struct MyRose_Tip { T f0; };
template <typename T> struct MyRose_Branch { T f0; std::vector<MyRose<T>> f1; };

inline int rose_size(const MyRose<int>& t) {
    if (auto p = std::get_if<std::shared_ptr<MyRose_Branch<int>>>(&t.v)) {
        int n = 1;
        for (const auto& k : (*p)->f1) n += rose_size(k);
        return n;
    }
    return 1;
}

inline int depth(const MyTree<int>& t) {
    if (auto p = std::get_if<std::shared_ptr<MyTree_Node<int>>>(&t.v))
        return 1 + std::max(depth((*p)->f0), depth((*p)->f1));
    return 1;
}

inline MyBox<int> grow(MyBox<int> b) {
    if (auto p = std::get_if<std::shared_ptr<MyBox_Full<int>>>(&b.v))
        return MyBox<int>{std::make_shared<MyBox_Full<int>>(MyBox_Full<int>{(*p)->f0 + 1})};
    return b;
}

inline MyBox<std::string> shout(MyBox<std::string> b) {
    if (auto p = std::get_if<std::shared_ptr<MyBox_Full<std::string>>>(&b.v))
        return MyBox<std::string>{std::make_shared<MyBox_Full<std::string>>(MyBox_Full<std::string>{(*p)->f0 + "!"})};
    return b;
}

inline MyWrap<int> bump(MyWrap<int> w) { return MyWrap<int>{w.item + 1}; }

inline int inc(int x) { return x + 1; }
