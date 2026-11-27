#ifndef __FOO_HPP__
#define __FOO_HPP__

#include <vector>
#include <algorithm>
#include <functional>
#include <type_traits>
#include <utility>

template <class A, class F>
auto map(F f, const std::vector<A> &xs) -> std::vector<std::invoke_result_t<F, A>> {
    using B = std::invoke_result_t<F, A>;
    std::vector<B> ys(xs.size());
    std::transform(xs.begin(), xs.end(), ys.begin(), f);
    return ys;
}

template <class A, class B>
B bar(B dummy, const std::tuple<A,B> &x){
    return(std::get<1>(x));
}

#endif
