#ifndef __COLLECT_THREADS_STDOUT_IMPL_HPP__
#define __COLLECT_THREADS_STDOUT_IMPL_HPP__

#include <cstdint>
#include <thread>
#include <vector>

template <typename F>
void produce_threads(F sink) {
    std::vector<std::thread> ts;
    for (int64_t t = 0; t < 4; t++) {
        ts.emplace_back([&sink, t]() {
            for (int64_t b = 0; b < 5; b++) {
                std::vector<int64_t> v;
                for (int64_t k = 0; k <= b; k++) v.push_back(t * 100 + k);
                sink(v);
            }
        });
    }
    for (auto& t : ts) t.join();
}

#endif
