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

template <class A, class B, class C, class F>
std::vector<C> czipWith(
        F f,
        const std::vector<A>& xs,
        const std::vector<B>& ys
    )
{
    static_assert(std::is_invocable_r_v<C, F, A, B>, 
                  "Function f must be callable with type A and return type B");
    std::size_t N = std::min(xs.size(), ys.size());
    std::vector<C> zs(N);
    for(std::size_t i = 0; i < N; i++){
        zs[i] = f(xs[i], ys[i]);
    }
    return zs;
}

#endif
