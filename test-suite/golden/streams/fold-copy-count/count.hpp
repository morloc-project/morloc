#ifndef __FOLD_COPY_COUNT_HPP__
#define __FOLD_COPY_COUNT_HPP__

#include <atomic>
#include <cstdlib>
#include <functional>
#include <new>
#include <vector>

// The accumulator is 100000 ints (400 KB); a batch is 8 ints. Anything this
// large is the accumulator or a copy of it.
constexpr std::size_t BIG_ALLOC = 64 * 1024;
inline std::atomic<long> g_big_allocs{0};

void* operator new(std::size_t n) {
    if (n >= BIG_ALLOC) g_big_allocs.fetch_add(1, std::memory_order_relaxed);
    if (void* p = std::malloc(n ? n : 1)) return p;
    throw std::bad_alloc();
}
void operator delete(void* p) noexcept { std::free(p); }
void operator delete(void* p, std::size_t) noexcept { std::free(p); }

inline void produce(int n, std::function<void(std::vector<int>)> sink) {
    for (int i = 0; i < n; i++) sink(std::vector<int>(8, i));
}

inline std::vector<int> initAcc() { return std::vector<int>(100000, 0); }

// One copy per call, by design: the step takes its accumulator by reference
// and returns a new one.
inline std::vector<int> step(const std::vector<int>& acc, const std::vector<int>& xs) {
    std::vector<int> r = acc;
    r[0] += (int)xs.size();
    return r;
}

inline std::vector<int> comb(const std::vector<int>& a, const std::vector<int>& b) {
    std::vector<int> r = a;
    r[0] += b[0];
    return r;
}

inline int report(const std::vector<int>&) {
    return (int)g_big_allocs.load();
}

#endif
