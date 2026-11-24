#ifndef __SPEC_STRM_7_1_HPP__
#define __SPEC_STRM_7_1_HPP__

#include <string>
#include <vector>

template <typename F>
void produceCpp(int n, F sink) {
    for (int i = 0; i < n; i++) {
        sink(std::vector<int>{i, i * 10});
    }
}

inline std::string joinAll(const std::vector<int>& xs) {
    std::string out;
    for (size_t i = 0; i < xs.size(); i++) {
        if (i > 0) out += ",";
        out += std::to_string(xs[i]);
    }
    return out + "\n";
}

#endif
