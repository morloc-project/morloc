#pragma once
#include <memory>

template <class T> struct list_t {
  T val;
  std::shared_ptr<const list_t<T>> next;
};

inline list_t<int> mk_list(int k) {
  list_t<int> head{k, nullptr};
  for (int i = k - 1; i >= 1; --i) {
    head = list_t<int>{i, std::make_shared<const list_t<int>>(head)};
  }
  return head;
}

inline int len_list(const list_t<int>& l) {
  int n = 1;
  for (auto p = l.next; p; p = p->next) ++n;
  return n;
}

inline int sum_list(const list_t<int>& l) {
  int s = l.val;
  for (auto p = l.next; p; p = p->next) s += p->val;
  return s;
}
