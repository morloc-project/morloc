#pragma once
#include <fstream>
#include <string>
#include <vector>

inline std::vector<int> readLines(const std::string& path) {
    std::vector<int> xs;
    int x;
    std::ifstream in(path);
    while (in >> x) xs.push_back(x);
    return xs;
}

template <class Sink>
void streamLines(const std::string& path, Sink sink) {
    sink(readLines(path));
}

template <class Sink>
void emitList(const std::vector<int>& xs, Sink sink) {
    sink(xs);
}

template <class Sink>
void produce(Sink sink) {
    sink(std::vector<int>{100});
}

inline int sumC(const std::vector<int>& xs) {
    int s = 0;
    for (int x : xs) s += x;
    return s;
}

inline std::string showList(const std::vector<int>& xs) {
    std::string s;
    for (int x : xs) s += (s.empty() ? "" : ",") + std::to_string(x);
    return "[" + s + "]";
}

inline std::string tagged(const std::string& t, int x) { return t + std::to_string(x) + "\n"; }

inline std::string listed(const std::vector<int>& xs, int x) {
    return showList(xs) + " sums to " + std::to_string(x) + "\n";
}

inline std::string batchWith(const std::vector<int>& xs, const std::vector<int>& batch) {
    return "input " + showList(xs) + " batch " + showList(batch) + "\n";
}

inline std::string countWith(int n, int x) {
    return std::to_string(n) + " elements, total " + std::to_string(x) + "\n";
}
