#pragma once

#include <string>
#include <vector>

struct foo_t {
    std::vector<int> flooz;
    std::vector<std::string> things;
    int size;
};

foo_t addFlooz(int flooz, foo_t foo){
    foo.flooz.push_back(flooz);
    return foo;
}
