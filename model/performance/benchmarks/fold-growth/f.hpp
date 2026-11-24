#pragma once
#include <string>
#include <vector>

template <typename F>
void produce(int64_t s, int64_t b, F sink) {
    for (int64_t i = 0; i < b; i++) {
        std::vector<int64_t> xs;
        for (int64_t k = 0; k < s; k++) xs.push_back(i * s + k);
        sink(xs);
    }
}

inline std::vector<int64_t> empty() { return {}; }

inline std::vector<int64_t> step_val(std::vector<int64_t> acc, const std::vector<int64_t>& xs) {
    acc.insert(acc.end(), xs.begin(), xs.end());
    return acc;
}

inline std::vector<int64_t> cat(std::vector<int64_t> a, const std::vector<int64_t>& b) {
    a.insert(a.end(), b.begin(), b.end());
    return a;
}

inline std::string len_of(const std::vector<int64_t>& a) { return std::to_string(a.size()); }

inline std::vector<int64_t> ident(const std::vector<int64_t>& xs) { return xs; }
