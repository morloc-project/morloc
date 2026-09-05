#pragma once
#include <string>
#include <vector>

template <class A>
struct Box {
  std::vector<A> v;
  std::string how;
};

template <class A>
Box<A> pack_box(const std::vector<A>& xs) {
  Box<A> b;
  b.v = xs;
  b.how = "cpp-generic";
  return b;
}

template <class A>
std::vector<A> unpack_box(const Box<A>& b) {
  return b.v;
}

inline Box<int> pack_box_int(const std::vector<int>& xs) {
  Box<int> b;
  b.v = xs;
  b.how = "cpp-int";
  return b;
}

inline std::vector<int> unpack_box_int(const Box<int>& b) { return b.v; }

template <class A>
std::string how_box(const Box<A>& b) {
  return b.how;
}

template <class A>
A box_sum(const Box<A>& b) {
  A s = 0;
  for (auto& e : b.v) s += e;
  return s;
}
