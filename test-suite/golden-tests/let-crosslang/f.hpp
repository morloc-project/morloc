#ifndef __F_HPP__
#define __F_HPP__

#include <string>
#include <vector>

std::string makeStr(int n) {
    return std::string(n, 'x');
}

int strLen(const std::string& s) {
    return (int)s.size();
}

int doubleIt(int x) {
    return x * 2;
}

int addTwo(int a, int b) {
    return a + b;
}

std::vector<int> makeRange(int n) {
    std::vector<int> result(n);
    for (int i = 0; i < n; i++) result[i] = i + 1;
    return result;
}

int sumList(const std::vector<int>& xs) {
    int total = 0;
    for (int x : xs) total += x;
    return total;
}

std::vector<int> incAll(const std::vector<int>& xs) {
    std::vector<int> result(xs.size());
    for (size_t i = 0; i < xs.size(); i++) result[i] = xs[i] + 1;
    return result;
}

std::string idStr(const std::string& s) {
    return s;
}

std::vector<int> idList(const std::vector<int>& xs) {
    return xs;
}

int counter() {
    static int n = 0;
    return ++n;
}

#endif
