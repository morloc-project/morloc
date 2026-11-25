#pragma once
#include <cstdint>
#include <cstdio>
#include <sys/wait.h>
#include <unistd.h>

struct outer_t { double v; };

inline double packInner(int64_t x) { return double(x); }

inline int64_t unpackInner(double x) {
    FILE* f = std::fopen("calls.log", "a");
    std::fprintf(f, "unpackInner\n");
    std::fclose(f);
    return int64_t(x);
}

inline outer_t packOuter(double x) { return {x}; }

inline double unpackOuter(outer_t o) {
    pid_t pid = fork();
    if (pid == 0) return o.v;
    waitpid(pid, nullptr, 0);
    return o.v;
}

inline outer_t mk(int64_t n) { return {double(n)}; }
