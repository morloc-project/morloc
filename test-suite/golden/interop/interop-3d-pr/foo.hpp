#ifndef MORLOC_FOO_HPP
#define MORLOC_FOO_HPP

#include <vector>
#include <algorithm>
#include <functional>
#include <utility>

int cneg(int x){
    return (-1) * x;
}

int cadd(int x, int y){
    return x + y;
}

int cmul(int x, int y){
    return x * y;
}

template <class A, class B, class F>
std::vector<B> cmap(F f, const std::vector<A>& xs) {
    static_assert(std::is_invocable_r_v<B, F, A>, 
                  "Function f must be callable with type A and return type B");
    
    std::vector<B> ys;
    ys.reserve(xs.size());
    for(const auto& x : xs) {
        ys.push_back(f(x));
    }
    return ys;
}

#endif
