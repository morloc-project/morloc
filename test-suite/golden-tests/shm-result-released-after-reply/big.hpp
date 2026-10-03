#ifndef __BIG_HPP__
#define __BIG_HPP__
#include <cstdint>
#include <vector>

inline std::vector<int> big(int n) {
    return std::vector<int>(n, 7);
}

inline int count(const std::vector<int>& xs) {
    return (int)xs.size();
}

inline int64_t heldAfterCount(int n) {
    (void)n;
    return morloc_shm_live_bytes();
}
#endif
