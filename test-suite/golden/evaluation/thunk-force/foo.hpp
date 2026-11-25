#ifndef __FOO_HPP__
#define __FOO_HPP__

#include <iostream>

int sideEffect(int x) {
    std::cout << "EVAL " << x << std::endl;
    return x * 2;
}

#endif
