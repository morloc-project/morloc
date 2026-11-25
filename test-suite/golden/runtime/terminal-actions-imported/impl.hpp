#pragma once
#include <functional>
#include <string>
#include <vector>

inline void produce(std::function<void(const std::vector<int>&)> k) {
    k({1, 2});
    k({3, 4, 5});
}

inline std::string showBatch(const std::vector<int>& xs) {
    return "batch of " + std::to_string(xs.size()) + "\n";
}

inline std::string otherBatch(const std::vector<int>& xs) {
    return "other batch of " + std::to_string(xs.size()) + "\n";
}

inline std::vector<int> range(int n) {
    std::vector<int> xs;
    for (int i = 1; i <= n; i++) xs.push_back(i);
    return xs;
}

inline std::string showList(const std::vector<int>& xs) {
    std::string s = "list:";
    for (int x : xs) s += " " + std::to_string(x);
    return s + "\n";
}
