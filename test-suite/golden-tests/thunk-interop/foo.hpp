#ifndef __FOO_HPP__
#define __FOO_HPP__

#include <iostream>

int sideEffectCpp(int x) {
    std::cout << "EVAL_CPP " << x << std::endl;
    return x * 2;
}

int addCpp(int a, int b) {
    return a + b;
}

#endif
