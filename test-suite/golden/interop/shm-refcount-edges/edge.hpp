#ifndef __EDGE_HPP__
#define __EDGE_HPP__

#include <string>
#include <vector>

int counter() {
    static int n = 0;
    return ++n;
}

int addTwo(int a, int b) {
    return a + b;
}

int doubleIt(int x) {
    return x * 2;
}

std::vector<int> makePair(int a, int b) {
    return {a, b};
}

std::string replicateStr(int n, const std::string& s) {
    std::string result;
    for (int i = 0; i < n; i++) result += s;
    return result;
}

int lenStr(const std::string& s) {
    return (int)s.size();
}

int sumVec(const std::vector<int>& xs) {
    int s = 0;
    for (int x : xs) s += x;
    return s;
}

std::vector<int> appendVec(const std::vector<int>& a, const std::vector<int>& b) {
    std::vector<int> result = a;
    result.insert(result.end(), b.begin(), b.end());
    return result;
}

std::vector<int> incVec(const std::vector<int>& xs) {
    std::vector<int> result(xs.size());
    for (size_t i = 0; i < xs.size(); i++) result[i] = xs[i] + 1;
    return result;
}

#endif
