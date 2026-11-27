#pragma once
#include <cstdint>
#include <cstdio>
#include <tuple>
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

struct p_t { double a, b, c; };
inline p_t packP(const std::tuple<double, double, double>& t) { return {std::get<0>(t), std::get<1>(t), std::get<2>(t)}; }
inline std::tuple<double, double, double> unpackP(const p_t& p) {
    FILE* f = std::fopen("calls.log", "a");
    std::fprintf(f, "unpackP\n");
    std::fclose(f);
    return {p.a, p.b, p.c};
}
inline p_t mkP(int64_t n) { return {double(n), double(n + 1), double(n + 2)}; }
