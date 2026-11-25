#pragma once
#include <string>
#include <vector>

int inc(int x) { return x + 1; }

int add(int x, int y) { return x + y; }

std::string join2(std::string x, std::string y) { return x + y; }

int count2(std::vector<int> xs) { return xs.size(); }

std::vector<int> ident(std::vector<int> xs) { return xs; }

std::vector<std::string> mknames(int n) {
    std::vector<std::string> xs;
    for (int i = 0; i < n; i++) xs.push_back("n" + std::to_string(i));
    return xs;
}
