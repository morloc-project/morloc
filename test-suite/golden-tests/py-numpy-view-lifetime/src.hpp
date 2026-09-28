#ifndef __PY_NUMPY_VIEW_LIFETIME_HPP__
#define __PY_NUMPY_VIEW_LIFETIME_HPP__

#include <vector>

// 20000 doubles (160 KB): past the inline threshold, so it travels by
// reference and Python reads it as a view onto shared memory.
inline std::vector<double> bigVec(int i) {
    std::vector<double> xs(20000);
    for (int j = 0; j < 20000; j++) xs[j] = (double)(i + 1);
    return xs;
}

inline std::vector<double> triple(int i) {
    return std::vector<double>{(double)i, (double)i + 0.5, 1.0};
}

#endif
