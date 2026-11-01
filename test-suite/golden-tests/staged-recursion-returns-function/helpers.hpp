#pragma once
#include <cstdlib>
#include <fstream>
#include <string>

template <class F>
int apply2(F f, int x) { return f(x) + f(x); }

inline int tick(int x) {
    std::ofstream f(std::getenv("TICK_LOG"), std::ios::app);
    f << "tick\n";
    return x;
}

inline int ticks(int) {
    std::ifstream f(std::getenv("TICK_LOG"));
    std::string line;
    int n = 0;
    while (std::getline(f, line)) n++;
    return n;
}
