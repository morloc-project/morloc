#ifndef __TERMINAL_ACTIONS_BRANCHES_HPP__
#define __TERMINAL_ACTIONS_BRANCHES_HPP__

#include <string>
#include <tuple>
#include <vector>

template <typename F>
void produce(F sink) {
    sink(std::vector<int>{1, 2});
    sink(std::vector<int>{3, 4, 5});
}

template <typename F>
void produceTail(F sink) {
    sink(std::vector<int>{7});
}

template <typename F>
void produceUpTo(int n, F sink) {
    std::vector<int> xs;
    for (int i = 1; i <= n; i++) xs.push_back(i);
    sink(xs);
}

inline void nothing() {}

inline std::string joinAll(const std::vector<int>& xs) {
    std::string out = std::to_string(xs.size()) + ":";
    for (int x : xs) out += " " + std::to_string(x);
    return out + "\n";
}

inline std::string showBatch(const std::vector<int>& xs) {
    return "batch of " + std::to_string(xs.size()) + "\n";
}

using Acc = std::tuple<int, int>;

inline Acc zero() { return Acc(0, 0); }

inline Acc addBatch(const Acc& acc, const std::vector<int>& xs) {
    int sum = std::get<0>(acc);
    for (int x : xs) sum += x;
    return Acc(sum, std::get<1>(acc) + 1);
}

inline Acc merge(const Acc& a, const Acc& b) {
    return Acc(std::get<0>(a) + std::get<0>(b), std::get<1>(a) + std::get<1>(b));
}

inline std::string showAcc(const Acc& acc) {
    return "sum=" + std::to_string(std::get<0>(acc)) + " batches=" + std::to_string(std::get<1>(acc)) + "\n";
}

#endif
