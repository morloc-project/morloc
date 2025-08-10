#pragma once

#include <string>
#include <vector>

struct bar_t {
    std::vector<std::string> things;
    int size;
};

struct foo_t {
    std::vector<bar_t> bars;
    std::vector<std::string> things;
    int size;
};


foo_t addBar(bar_t bar, foo_t foo){
    foo.bars.push_back(bar);
    return foo;
}
