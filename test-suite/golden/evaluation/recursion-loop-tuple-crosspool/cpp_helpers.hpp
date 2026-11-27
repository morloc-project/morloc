#pragma once

#include <vector>
#include <tuple>

int cpp_dec(int a) {
    return a - 1;
}

// Native (in-pool) tuple-returning step, so an <IO> loop that also performs a
// foreign side effect stays a same-pool native loop.
std::tuple<std::vector<int>, int> cpp_step(std::vector<int> xs) {
    int head = xs.empty() ? 0 : xs[0];
    std::vector<int> rest(xs.begin() + (xs.empty() ? 0 : 1), xs.end());
    return std::make_tuple(rest, head);
}
