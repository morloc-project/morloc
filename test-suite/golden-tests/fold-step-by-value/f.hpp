#pragma once
#include <atomic>
#include <chrono>
#include <cstdint>
#include <functional>
#include <thread>
#include <vector>

constexpr std::size_t SPARE = 1 << 16;

inline std::atomic<int> g_entered{0};

// Each thread's first step waits until another thread's step has started.
inline void meet() {
    thread_local bool waited = false;
    if (waited) return;
    waited = true;
    g_entered.fetch_add(1);
    auto start = std::chrono::steady_clock::now();
    while (g_entered.load() < 2 && std::chrono::steady_clock::now() - start < std::chrono::seconds(10))
        std::this_thread::yield();
}

inline void produce(int64_t n, std::function<void(std::vector<int64_t>)> sink) {
    for (int64_t i = 0; i < n; i++) sink(std::vector<int64_t>{i});
}

inline void produce2(int64_t n, std::function<void(std::vector<int64_t>)> sink) {
    std::thread ts[2];
    for (int64_t t = 0; t < 2; t++)
        ts[t] = std::thread([&, t] { for (int64_t i = 0; i < n; i++) sink(std::vector<int64_t>{t * n + i}); });
    for (auto& t : ts) t.join();
}

inline std::vector<int64_t> init_acc() { return {0}; }

inline std::vector<int64_t> step(std::vector<int64_t> acc, const std::vector<int64_t>&) {
    meet();
    if (acc.capacity() < SPARE) {
        acc[0] += 1;
        acc.reserve(SPARE);
    }
    return acc;
}

inline std::vector<int64_t> comb(std::vector<int64_t> a, std::vector<int64_t> b) {
    int64_t fresh = (a.capacity() < SPARE) + (b.capacity() < SPARE);
    a[0] += b[0] + 100 * fresh;
    return a;
}

inline int64_t fresh(const std::vector<int64_t>& acc) { return acc[0]; }

inline std::vector<int64_t> bump(const std::vector<int64_t>& xs) {
    std::vector<int64_t> r;
    for (auto x : xs) r.push_back(x + 1);
    return r;
}

