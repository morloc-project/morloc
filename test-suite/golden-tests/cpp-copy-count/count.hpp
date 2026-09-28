#ifndef __CPP_COPY_COUNT_HPP__
#define __CPP_COPY_COUNT_HPP__

#include <atomic>
#include <cstdlib>
#include <new>
#include <vector>

// A batch is 100000 ints (400 KB); anything this large is a batch or a copy
// of one.
constexpr std::size_t BIG_ALLOC = 64 * 1024;
inline std::atomic<long> g_big_allocs{0};

void* operator new(std::size_t n) {
    if (n >= BIG_ALLOC) g_big_allocs.fetch_add(1, std::memory_order_relaxed);
    if (void* p = std::malloc(n ? n : 1)) return p;
    throw std::bad_alloc();
}
void operator delete(void* p) noexcept { std::free(p); }
void operator delete(void* p, std::size_t) noexcept { std::free(p); }

inline std::vector<int> makeBatch(int n) {
    return std::vector<int>(100000, n);
}

inline long g_sum = 0;

inline std::vector<int> makeExtra(int k) {
    return std::vector<int>(100000, k);
}

inline void recordBatch(const std::vector<int>& extra, const std::vector<int>& xs) {
    g_sum += (long)extra.size() * (long)xs.size();
}

inline int bigAllocs(int) {
    return (int)g_big_allocs.load();
}

#endif
