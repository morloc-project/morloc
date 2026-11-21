#pragma once
#include <cstdint>
#include <cstdio>
#include <vector>

struct outer_t { std::vector<double> v; };

inline double packInner(int64_t x) { return double(x); }
inline int64_t unpackInner(double x) { return int64_t(x); }

inline outer_t packOuter(const std::vector<double>& v) { return {v}; }
inline std::vector<double> unpackOuter(const outer_t& o) {
    FILE* f = std::fopen("calls.log", "a");
    std::fprintf(f, "unpackOuter\n");
    std::fclose(f);
    return o.v;
}

inline outer_t mk(int64_t n) {
    outer_t o;
    for (int64_t i = 0; i < n; i++) o.v.push_back(double(i));
    return o;
}
