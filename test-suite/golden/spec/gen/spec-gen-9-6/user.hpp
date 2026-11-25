#pragma once
#include <memory>
#include <string>
template <class A> A same(const A& x) { return x; }
template <class T> struct list_t {
  T val;
  std::shared_ptr<const list_t<T>> next;
};
inline int innerL(const list_t<list_t<int>>& l) { return l.val.val; }
