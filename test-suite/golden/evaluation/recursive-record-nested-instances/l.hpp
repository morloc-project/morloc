#pragma once
#include <memory>
#include <string>
template <class T> struct list_t {
  T val;
  std::shared_ptr<const list_t<T>> next;
  std::shared_ptr<const list_t<std::string>> s;
  std::shared_ptr<const list_t<int>> i;
};
template <class A> A same(const A& x) { return x; }
