#pragma once
#include <thread>
#include <vector>

template <class F>
long rounds(F f, int k) {
    long total = 0;
    for (int r = 0; r < k; ++r) {
        std::vector<int> out(8);
        std::vector<std::thread> ts;
        for (int t = 0; t < 8; ++t) ts.emplace_back([&, t] { out[t] = f(r * 8 + t); });
        for (auto& t : ts) t.join();
        for (int v : out) total += v;
    }
    return total;
}
