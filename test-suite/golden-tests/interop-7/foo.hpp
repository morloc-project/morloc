#ifndef __FOO_HPP__
#define __FOO_HPP__

#include <algorithm>
#include <assert.h>
#include <functional>
#include <iostream>
#include <map>
#include <utility>
#include <variant>
#include <vector>

// foldTree :: (l -> a -> a)
//          -> (n -> e -> a -> a)
//          -> a
//          -> (n, e, l)
//          -> a
template<typename N, typename E, typename L, typename A>
A g(
  std::function<A(L,A)> laa,
  std::function<A(N,E,A)> anea,
  A b,
  std::tuple<N,E,L> x
){
    A a1 = laa(std::get<2>(x), b);
    A a2 = anea(std::get<0>(x), std::get<1>(x), a1);
    return a2;
}

#endif
