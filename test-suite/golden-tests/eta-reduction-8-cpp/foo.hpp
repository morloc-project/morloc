#ifndef __FOO_HPP__
#define __FOO_HPP__

#include <functional>
#include <utility>

template<class A, class B, class C, class D>
D onThree(std::function<D(A, B, C)> f, std::tuple<A, B, C> x){
    return f(std::get<0>(x), std::get<1>(x), std::get<2>(x)); 
}

int inc(int x){
    return x + 1;
}

#endif
