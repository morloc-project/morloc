#pragma once
#include <fstream>
#include <string>
#include <vector>

template <class Sink>
void produceC(const std::string& path, Sink sink) {
    std::ifstream in(path);
    std::vector<int> batch;
    std::string line;
    while (std::getline(in, line)) batch.push_back(std::stoi(line));
    sink(batch);
}

inline int sumC(const std::vector<int>& xs) { int s = 0; for (int x : xs) s += x; return s; }

inline std::string showWith(const std::string& tag, int x) { return tag + std::to_string(x) + "\n"; }
