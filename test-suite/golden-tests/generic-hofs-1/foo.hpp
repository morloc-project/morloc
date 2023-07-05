#ifndef __FOO_HPP__
#define __FOO_HPP__

#include <vector>
#include <algorithm>
#include <functional>
#include <utility>

template <class A, class B>
std::vector<B> map(std::function<B(A)> f, const std::vector<A> &xs){
    std::vector<B> ys(xs.size());
    std::transform(xs.begin(), xs.end(), ys.begin(), f);
    return ys;
}

template <class A, class B>
B snd(const std::tuple<A,B> &x){
    return(std::get<1>(x));
}

#endif
