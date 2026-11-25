#ifndef __FOO_HPP__
#define __FOO_HPP__

#include <string>

std::string combine(std::string name, int value) {
    return name + "_" + std::to_string(value);
}

int add(int a, int b) {
    return a + b;
}

#endif
