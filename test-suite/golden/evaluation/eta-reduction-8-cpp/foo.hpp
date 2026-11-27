#ifndef __FOO_HPP__
#define __FOO_HPP__

#include <functional>
#include <type_traits>
#include <utility>

template<class A, class B, class C, class F>
auto onThree(F f, const std::tuple<A, B, C>& x) -> std::invoke_result_t<F, A, B, C> {
    return f(std::get<0>(x), std::get<1>(x), std::get<2>(x));
}

int inc(int x){
    return x + 1;
}

#endif
