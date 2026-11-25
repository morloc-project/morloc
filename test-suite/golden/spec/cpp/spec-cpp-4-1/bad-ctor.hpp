#ifndef __SPEC_CPP_4_1_CTOR__
#define __SPEC_CPP_4_1_CTOR__

#include <vector>

struct rec_t {
    std::vector<int> xs;
    int k;
    rec_t(std::vector<int>& v, int k_) : xs(v), k(k_) {}
};

inline std::vector<int> mk(int n) { return std::vector<int>(n, 1); }
inline int total(const rec_t& r) { return (int)r.xs.size() + r.k; }

#endif
