#pragma once
#include <string>

struct point_t {
    double x;
    double y;
};

struct het_t {
    std::string zed;
    int alpha;
};

struct nest_t {
    point_t inner;
    std::string tag;
};
