#ifndef MORLOC_FOO_HPP
#define MORLOC_FOO_HPP

#include <vector>
#include <algorithm>
#include <functional>
#include <type_traits>
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

template <class A, class F>
auto cmap(F f, const std::vector<A>& xs) -> std::vector<std::invoke_result_t<F, A>> {
    using B = std::invoke_result_t<F, A>;
    std::vector<B> ys;
    ys.reserve(xs.size());
    for(const auto& x : xs) {
        ys.push_back(f(x));
    }
    return ys;
}

template <class A, class B, class F>
auto czipWith(
        F f,
        const std::vector<A>& xs,
        const std::vector<B>& ys
    ) -> std::vector<std::invoke_result_t<F, A, B>>
{
    using C = std::invoke_result_t<F, A, B>;
    std::size_t N = std::min(xs.size(), ys.size());
    std::vector<C> zs(N);
    for(std::size_t i = 0; i < N; i++){
        zs[i] = f(xs[i], ys[i]);
    }
    return zs;
}

#endif
