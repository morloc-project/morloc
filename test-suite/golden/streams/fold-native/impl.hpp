#ifndef __FOLD_NATIVE_IMPL_HPP__
#define __FOLD_NATIVE_IMPL_HPP__

#include <cstdint>
#include <functional>
#include <string>
#include <thread>
#include <vector>
#include <sys/wait.h>
#include <unistd.h>

template <typename F>
void produce_none(F sink) {
    (void)sink;
}

template <typename F>
void produce_fork_none(F sink) {
    (void)sink;
    pid_t pid = fork();
    if (pid == 0) _exit(0);
    int st = 0;
    waitpid(pid, &st, 0);
}

// Four threads, five batches each; batch b of thread t is t*100 .. t*100+b.
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

inline std::function<void(const std::vector<int64_t>&)>& stored_sink() {
    static std::function<void(const std::vector<int64_t>&)> s;
    return s;
}

template <typename F>
void produce_store(F sink) {
    stored_sink() = [sink](const std::vector<int64_t>& v) { sink(v); };
    sink(std::vector<int64_t>{1, 2});
}

// Calls the sink a finished fold left behind: a late put must fail, and
// the caller can catch it.
inline std::string show_late(int64_t n) {
    std::string late = "no sink";
    if (stored_sink()) {
        try {
            stored_sink()(std::vector<int64_t>{7});
            late = "accepted";
        } catch (...) {
            late = "failed";
        }
    }
    return "cpp n=" + std::to_string(n) + " late put " + late + "\n";
}

inline int64_t add_sum(int64_t acc, const std::vector<int64_t>& xs) {
    for (auto x : xs) acc += x;
    return acc;
}

inline int64_t merge(int64_t a, int64_t b) { return a + b; }

inline std::string show_n(int64_t n) { return "cpp n=" + std::to_string(n) + "\n"; }

#endif
