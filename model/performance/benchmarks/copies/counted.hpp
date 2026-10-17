#ifndef __COPY_CENSUS_CPP_HPP__
#define __COPY_CENSUS_CPP_HPP__

#include <atomic>
#include <cstdlib>
#include <functional>
#include <new>
#include <tuple>
#include <vector>

// A payload holds 20000 + key ints (80 KB or more); an allocation this large
// is either a payload a builder made or a copy of one.
constexpr std::size_t BIG_ALLOC = 64 * 1024;
constexpr int BASE = 20000;
inline std::atomic<long> g_big_allocs{0};
inline std::atomic<long> g_built{0};

void* operator new(std::size_t n) {
    if (n >= BIG_ALLOC) g_big_allocs.fetch_add(1, std::memory_order_relaxed);
    if (void* p = std::malloc(n ? n : 1)) return p;
    throw std::bad_alloc();
}
void operator delete(void* p) noexcept { std::free(p); }
void operator delete(void* p, std::size_t) noexcept { std::free(p); }

inline std::vector<int> mkL(int key) {
    g_built.fetch_add(1, std::memory_order_relaxed);
    return std::vector<int>(BASE + key, key);
}

inline std::vector<int> grow(const std::vector<int>&, int key) { return mkL(key); }

inline int keyL(const std::vector<int>& v) { return (int)v.size() - BASE; }
inline int keyV(std::vector<int> v) { return (int)v.size() - BASE; }

inline int applyF(std::function<int(int)> f, int x) { return f(x); }
inline int applyR(const std::function<int(int)>& f, int x) { return f(x); }

// The value it is given, then the large allocations no builder made.
inline std::tuple<int, int> census(int v) {
    return std::make_tuple(v, (int)(g_big_allocs.load() - g_built.load()));
}

#endif
