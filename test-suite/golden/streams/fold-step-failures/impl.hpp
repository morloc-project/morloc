#ifndef __FOLD_STEP_FAILURES_IMPL_HPP__
#define __FOLD_STEP_FAILURES_IMPL_HPP__

#include <atomic>
#include <cstdint>
#include <cstdlib>
#include <new>
#include <string>
#include <vector>

// While starved, an allocation as large as the accumulator fails.
constexpr std::size_t BIG_ALLOC = 64 * 1024;
inline std::atomic<bool> g_starved{false};

void* operator new(std::size_t n) {
    if (n >= BIG_ALLOC && g_starved.load()) throw std::bad_alloc();
    if (void* p = std::malloc(n ? n : 1)) return p;
    throw std::bad_alloc();
}
void operator delete(void* p) noexcept { std::free(p); }
void operator delete(void* p, std::size_t) noexcept { std::free(p); }

inline std::vector<int64_t> big() { return std::vector<int64_t>(100000, 0); }

// The first put copies init, which fails; the producer catches the error.
template <typename F>
void produce_starved(F sink) {
    g_starved = true;
    try {
        sink(std::vector<int64_t>{1});
    } catch (...) {
    }
    g_starved = false;
}

inline std::vector<int64_t> add_len(std::vector<int64_t> acc, const std::vector<int64_t>& xs) {
    acc.push_back((int64_t)xs.size());
    return acc;
}

inline std::vector<int64_t> merge_len(std::vector<int64_t> a, const std::vector<int64_t>& b) {
    a.insert(a.end(), b.begin(), b.end());
    return a;
}

inline std::string show_len(const std::vector<int64_t>& xs) { return "cpp len=" + std::to_string(xs.size()) + "\n"; }

#endif
