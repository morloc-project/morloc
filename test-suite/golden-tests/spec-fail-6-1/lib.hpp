#pragma once
#include <cstdlib>
#include <functional>

int die(int x) { std::_Exit(9); return x; }

int inc(int x) { return x + 1; }

int guard(std::function<int(int)> f, int x) {
    try {
        return f(x);
    } catch (...) {
        return -1;
    }
}
