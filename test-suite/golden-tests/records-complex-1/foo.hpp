#pragma once

#include <string>
#include <vector>

struct foo_t {
    std::map<std::string, int> shitlist;
    std::vector<std::string> things;
    int size;
};

foo_t addShit(std::string name, int level_of_shit, foo_t foo){
    foo.shitlist.insert({name, level_of_shit});
    return foo;
}
