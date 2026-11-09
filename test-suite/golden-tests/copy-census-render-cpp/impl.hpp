#ifndef __COPY_CENSUS_RENDER_CPP_HPP__
#define __COPY_CENSUS_RENDER_CPP_HPP__

#include <atomic>
#include <cstdlib>
#include <new>
#include <string>
#include <vector>

// A rendered body is 64 KiB or more; an allocation that large is either a
// body render made or a copy of one.
constexpr std::size_t BIG_ALLOC = 64 * 1024;
inline std::atomic<long> g_big_allocs{0};
inline std::atomic<long> g_built{0};

void* operator new(std::size_t n) {
    if (n >= BIG_ALLOC) g_big_allocs.fetch_add(1, std::memory_order_relaxed);
    if (void* p = std::malloc(n ? n : 1)) return p;
    throw std::bad_alloc();
}
void operator delete(void* p) noexcept { std::free(p); }
void operator delete(void* p, std::size_t) noexcept { std::free(p); }

inline std::vector<int> mk(int k) { return std::vector<int>(1, k); }

// A large body that begins with the copies of earlier bodies made so far.
inline std::string render(const std::vector<int>&) {
    long copies = g_big_allocs.load() - g_built.load();
    std::string s;
    g_built.fetch_add(1, std::memory_order_relaxed);
    s.reserve(BIG_ALLOC + 64);
    s += "copies=" + std::to_string(copies) + " ";
    s.append(BIG_ALLOC, 'x');
    s += "\n";
    return s;
}

#endif
