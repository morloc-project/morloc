#include <iostream>

int sideEffectCpp(int x) {
    std::cerr << "EVAL " << x << std::endl;
    return x * 2;
}
