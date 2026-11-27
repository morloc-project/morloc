#ifndef __HELPERS_HPP__
#define __HELPERS_HPP__

#include <iostream>

inline int cppEcho(int x) {
    std::cerr << "C" << x << std::endl;
    return x + 1;
}

inline int cppAdd(int a, int b) {
    return a + b;
}

#endif
