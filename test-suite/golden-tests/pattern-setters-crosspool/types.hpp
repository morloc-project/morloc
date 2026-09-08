#pragma once
#include <string>

struct pair_t {
    std::string zed;
    std::string alpha;
};

struct box_t {
    pair_t inner;
    int n;
};
