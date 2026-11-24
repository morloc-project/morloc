#ifndef __PFILTER_MOVE_HPP__
#define __PFILTER_MOVE_HPP__

#include <atomic>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <new>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

// Every allocation the pool makes is counted; batchEach reports those made
// inside its batch function.
inline std::atomic<long> pfm_allocs{0};

void* operator new(std::size_t n) {
    pfm_allocs.fetch_add(1, std::memory_order_relaxed);
    if (void* p = std::malloc(n ? n : 1)) return p;
    throw std::bad_alloc();
}
void operator delete(void* p) noexcept { std::free(p); }
void operator delete(void* p, std::size_t) noexcept { std::free(p); }

using pfm_rec = std::tuple<std::string, std::vector<std::uint8_t>>;

// n records in batches of 10000. Record i has a 24-byte name, so no string
// fits the small-string buffer, and a body of len bytes all equal to i % 100.
template <class Sink>
inline void genBatches(int n, int len, Sink sink) {
    const int per = 10000;
    for (int lo = 0; lo < n; lo += per) {
        std::vector<pfm_rec> batch;
        for (int i = lo; i < n && i < lo + per; ++i) {
            char name[32];
            std::snprintf(name, sizeof name, "record-%017d", i);
            batch.emplace_back(std::string(name), std::vector<std::uint8_t>(len, std::uint8_t(i % 100)));
        }
        sink(std::move(batch));
    }
}

// Keeps a record whose body starts below pct: pct 100 keeps all, 1 keeps 1%.
inline bool keepRec(int pct, const pfm_rec& r) {
    const auto& b = std::get<1>(r);
    return !b.empty() && int(b[0]) < pct;
}

// Pulls every batch and gives it to f, as a stream stage does. Returns the
// records f kept and the allocations made inside f.
template <class F, class Next>
inline std::tuple<int, int> batchEach(F f, Next next) {
    long kept = 0, allocs = 0;
    for (;;) {
        auto batch = next();
        if (batch.empty()) break;
        const long before = pfm_allocs.load(std::memory_order_relaxed);
#ifdef MLC_GIVE
        auto out = mlc::give(f, std::move(batch));
#else
        auto out = f(std::move(batch));
#endif
        allocs += pfm_allocs.load(std::memory_order_relaxed) - before;
        kept += long(out.size());
    }
    return std::make_tuple(int(kept), int(allocs));
}

// Its first argument; the second is read only to keep the batch alive.
template <class A>
inline std::vector<A> firstOf(std::vector<A>&& xs, int) { return std::move(xs); }
template <class A>
inline std::vector<A> firstOf(const std::vector<A>& xs, int) { return xs; }

#endif
