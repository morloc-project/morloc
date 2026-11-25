#ifndef __MORLOC__CORE_HPP__
#define __MORLOC__CORE_HPP__

#include <utility>

// fst      :: forall a b . (a, b) -> a ;
template <class A, class B>
A morloc_fst(std::tuple<A,B> x){
    return(std::get<0>(x));
}

template <class A>
bool morloc_eq(A x, A y){
   return x == y;
}

#endif
