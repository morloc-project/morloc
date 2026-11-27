#pragma once

#include <vector>

bool cpp_is_empty(std::vector<int> xs) {
    return xs.empty();
}

int cpp_head(std::vector<int> xs) {
    return xs[0];
}

std::vector<int> cpp_tail(std::vector<int> xs) {
    return std::vector<int>(xs.begin() + 1, xs.end());
}

int cpp_add(int a, int b) {
    return a + b;
}
