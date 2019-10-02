#ifndef __CORE_HPP__
#define __CORE_HPP__

#include <vector>
#include <algorithm>

template <class A, class B>
std::vector<B> map(B(*f)(A), const std::vector<A> &xs){
    std::vector<B> ys(xs.size());
    std::transform(xs.begin(), xs.end(), ys.begin(), f);
    return ys;
}

template <class A, class B>
B snd(A a, B b){
    return b;
}

#endif
