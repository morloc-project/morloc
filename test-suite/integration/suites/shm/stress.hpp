#ifndef MORLOC_STRESS_HPP
#define MORLOC_STRESS_HPP

#include <chrono>
#include <cstddef>
#include <thread>
#include <vector>

extern "C" {
    void* shmalloc(std::size_t size, char** errmsg);
    bool  shfree  (void* ptr, char** errmsg);
}

int sum_list(const std::vector<int>& xs) {
    int s = 0;
    for (auto x : xs) s += x;
    return s;
}

// Allocate `size_kb` KB via `libmorloc`'s SHM allocator, hold for
// `delay` seconds, then free. A `size_kb` larger than any existing
// volume (the initial `_0` is 64 KB and the stream registry `_32767`
// is ~2 MB) forces `shinit` of a fresh dynamic-growth volume, which
// persists on `/dev/shm/` until process shutdown.
int alloc_hold_and_sum(int size_kb, double delay) {
    char* err = nullptr;
    std::size_t n = static_cast<std::size_t>(size_kb) * 1024;
    void* p = shmalloc(n, &err);
    std::this_thread::sleep_for(std::chrono::duration<double>(delay));
    if (p != nullptr) {
        shfree(p, &err);
    }
    return size_kb;
}

#endif
