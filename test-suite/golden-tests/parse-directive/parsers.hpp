#pragma once
#include <fstream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

// "1,2,3" -> [1,2,3]
std::vector<int> readCsvCpp(const std::string& path) {
    std::ifstream in(path);
    if (!in) throw std::runtime_error("cannot open " + path);
    std::vector<int> xs;
    std::string field;
    while (std::getline(in, field, ',')) {
        std::size_t used = 0;
        int x = 0;
        try { x = std::stoi(field, &used); } catch (...) { used = 0; }
        if (used == 0) throw std::runtime_error("not a number: " + field);
        xs.push_back(x);
    }
    return xs;
}

// one integer per line, emitted in batches of two
template <class Sink>
void produceCpp(const std::string& path, Sink sink) {
    std::ifstream in(path);
    if (!in) throw std::runtime_error("cannot open " + path);
    std::vector<int> batch;
    int x;
    while (in >> x) {
        batch.push_back(x);
        if (batch.size() == 2) { sink(batch); batch.clear(); }
    }
    if (!batch.empty()) sink(batch);
}

int sumCpp(const std::vector<int>& xs) {
    int s = 0;
    for (int x : xs) s += x;
    return s;
}

int sizeCpp(const std::vector<int>& xs) { return (int)xs.size(); }
