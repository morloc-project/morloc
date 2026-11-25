#ifndef __CSTRESS_HPP__
#define __CSTRESS_HPP__

#include <string>
#include <vector>

std::string cmakeStr(int n) {
    return std::string(n, 'x');
}

int cstrLen(const std::string& s) {
    return (int)s.size();
}

int csumList(const std::vector<int>& xs) {
    int total = 0;
    for (int x : xs) total += x;
    return total;
}

std::vector<int> cmakeRange(int n) {
    std::vector<int> result(n);
    for (int i = 0; i < n; i++) result[i] = i + 1;
    return result;
}

std::vector<int> cincAll(const std::vector<int>& xs) {
    std::vector<int> result(xs.size());
    for (size_t i = 0; i < xs.size(); i++) result[i] = xs[i] + 1;
    return result;
}

std::string cidStr(const std::string& s) {
    return s;
}

std::vector<int> cidList(const std::vector<int>& xs) {
    return xs;
}

int cdoubleIt(int x) {
    return x * 2;
}

int caddTwo(int a, int b) {
    return a + b;
}

std::vector<int> cmakePair(int a, int b) {
    return {a, b};
}

std::string creplicateStr(int n, const std::string& s) {
    std::string result;
    for (int i = 0; i < n; i++) result += s;
    return result;
}

std::vector<int> cappendVec(const std::vector<int>& a, const std::vector<int>& b) {
    std::vector<int> result = a;
    result.insert(result.end(), b.begin(), b.end());
    return result;
}

std::vector<std::string> cvecOfStrs(int n, int m) {
    std::string s(m, 'B');
    return std::vector<std::string>(n, s);
}

int csumStrLens(const std::vector<std::string>& vs) {
    int total = 0;
    for (const auto& s : vs) total += (int)s.size();
    return total;
}

std::vector<std::vector<int>> cnestedVec(int outer, int inner) {
    std::vector<int> row(inner);
    for (int i = 0; i < inner; i++) row[i] = i + 1;
    return std::vector<std::vector<int>>(outer, row);
}

int cnestedSum(const std::vector<std::vector<int>>& vv) {
    int s = 0;
    for (const auto& v : vv)
        for (int x : v)
            s += x;
    return s;
}

int ccounter() {
    static int n = 0;
    return ++n;
}

#endif
