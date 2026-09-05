#pragma once
#include <tuple>
#include <vector>

template <class A>
struct BoxT {
  A a;
  A b;
};

template <class A>
BoxT<A> pack_tuple(const std::tuple<A, A>& t) {
  BoxT<A> x;
  x.a = std::get<0>(t);
  x.b = std::get<1>(t);
  return x;
}

template <class A>
std::tuple<A, A> unpack_tuple(const BoxT<A>& x) {
  return std::make_tuple(x.a, x.b);
}

template <class A>
struct BoxV {
  std::vector<A> v;
};

template <class A>
BoxV<A> pack_vec(const std::vector<A>& xs) {
  BoxV<A> x;
  x.v = xs;
  return x;
}

template <class A>
std::vector<A> unpack_vec(const BoxV<A>& x) {
  return x.v;
}

template <class A>
A sum_tuple(const BoxT<A>& x) {
  return x.a + x.b;
}

template <class A>
A sum_vec(const BoxV<A>& x) {
  A s = 0;
  for (auto& e : x.v) s += e;
  return s;
}
