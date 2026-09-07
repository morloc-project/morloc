#pragma once
#include <vector>

std::vector<int> cgen(int n) {
    std::vector<int> v;
    v.reserve(n);
    for (int i = 0; i < n; i++) {
        v.push_back(i);
    }
    return v;
}

int cadd(int a, int b) {
    return a + b;
}
