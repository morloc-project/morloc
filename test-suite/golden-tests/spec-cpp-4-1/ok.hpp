#ifndef __SPEC_CPP_4_1_OK__
#define __SPEC_CPP_4_1_OK__

#include <vector>

struct rec_t { std::vector<int> xs; int k; };
struct byval_t {
    std::vector<int> xs;
    int k;
    byval_t(std::vector<int> v, int k_) : xs(std::move(v)), k(k_) {}
};

inline std::vector<int> mk(int n) { return std::vector<int>(n, 1); }
inline int total(const rec_t& r) { return (int)r.xs.size() + r.k; }
inline int totalB(const byval_t& r) { return (int)r.xs.size() + r.k; }

#endif
