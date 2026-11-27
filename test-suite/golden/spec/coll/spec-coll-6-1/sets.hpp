#pragma once
#include <set>
#include <string>
#include <vector>
inline std::set<std::string> backward(int n) {
    std::vector<std::string> names = {"pear", "apple", "fig", "kiwi", "date"};
    std::set<std::string> s;
    for (int i = n - 1; i >= 0; i--) s.insert(names[i]);
    return s;
}
