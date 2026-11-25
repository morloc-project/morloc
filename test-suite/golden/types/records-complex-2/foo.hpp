#pragma once

#include <string>
#include <vector>

struct foo_t {
    std::map<std::string, std::map<std::string,int>> shitlists;
    std::vector<std::string> things;
    int size;
};

foo_t addShit(std::string name, int level_of_shit, foo_t foo){
    std::map<std::string,int> x;
    x.insert({"yolo", level_of_shit});
    foo.shitlists.insert({name, x});
    return foo;
}
