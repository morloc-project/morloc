#ifndef __POOL_KEPT_CONNECTIONS_HPP__
#define __POOL_KEPT_CONNECTIONS_HPP__
#include <functional>
#include <thread>
#include <vector>

inline int cppSum(std::function<int(int)> f, int n) {
    int total = 0;
    for (int i = 0; i < n; ++i) total += f(i);
    return total;
}

inline int cppApply(std::function<int(int)> f, int x) { return f(x); }

inline std::vector<int> cppPar(std::function<int(int)> f, const std::vector<int>& xs) {
    const size_t k = 4;
    std::vector<int> out(xs.size());
    std::vector<std::thread> ts;
    for (size_t t = 0; t < k; ++t) {
        ts.emplace_back([&, t] {
            for (size_t i = t; i < xs.size(); i += k) out[i] = f(xs[i]);
        });
    }
    for (auto& t : ts) t.join();
    return out;
}
#endif
