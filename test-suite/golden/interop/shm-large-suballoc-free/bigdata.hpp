#ifndef __BIGDATA_HPP__
#define __BIGDATA_HPP__

#include <string>
#include <vector>

std::string bigStr(int n) {
    return std::string(n, 'A');
}

int bigStrLen(const std::string& s) {
    return (int)s.size();
}

std::vector<int> bigVec(int n) {
    std::vector<int> v(n);
    for (int i = 0; i < n; i++) v[i] = i;
    return v;
}

int bigVecSum(const std::vector<int>& v) {
    long long s = 0;
    for (int x : v) s += x;
    return (int)(s % 1000000007);
}

std::vector<std::string> vecOfStrs(int n, int m) {
    std::string s(m, 'B');
    return std::vector<std::string>(n, s);
}

int sumStrLens(const std::vector<std::string>& vs) {
    int total = 0;
    for (const auto& s : vs) total += (int)s.size();
    return total;
}

std::vector<std::vector<int>> nestedVec(int outer, int inner) {
    std::vector<int> row(inner);
    for (int i = 0; i < inner; i++) row[i] = i + 1;
    return std::vector<std::vector<int>>(outer, row);
}

int nestedSum(const std::vector<std::vector<int>>& vv) {
    int s = 0;
    for (const auto& v : vv)
        for (int x : v)
            s += x;
    return s;
}

std::vector<std::string> idVecStr(const std::vector<std::string>& vs) {
    return vs;
}

std::vector<std::vector<int>> idNestedVec(const std::vector<std::vector<int>>& vv) {
    return vv;
}

#endif
