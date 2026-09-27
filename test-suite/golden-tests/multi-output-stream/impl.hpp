#ifndef __MULTI_OUTPUT_STREAM_HPP__
#define __MULTI_OUTPUT_STREAM_HPP__

#include <fstream>
#include <numeric>
#include <string>
#include <tuple>
#include <vector>

template <typename F>
void produce(const std::string& log, F sink) {
    std::ofstream(log, std::ios::app) << "ran\n";
    sink(std::vector<int>{1, 2});
    sink(std::vector<int>{});
    sink(std::vector<int>{3, 4, 5});
    std::vector<int> big(20000);
    std::iota(big.begin(), big.end(), 0);
    sink(big);
}

template <typename F>
void produceTail(F sink) {
    sink(std::vector<int>{7});
}

inline std::string joinAll(const std::vector<int>& xs) {
    std::string out = std::to_string(xs.size()) + ": ";
    for (size_t i = 0; i < xs.size() && i < 8; i++) {
        if (i > 0) out += ",";
        out += std::to_string(xs[i]);
    }
    return out + "\n";
}

inline std::string showBatch(const std::vector<int>& xs) {
    return "batch of " + std::to_string(xs.size()) + "\n";
}

using Acc = std::tuple<int, std::vector<int>>;

inline Acc zero() { return Acc(0, {}); }

inline Acc addBatch(const Acc& acc, const std::vector<int>& xs) {
    int sum = std::get<0>(acc);
    for (int x : xs) sum += x;
    std::vector<int> sizes = std::get<1>(acc);
    sizes.push_back((int)xs.size());
    return Acc(sum, sizes);
}

inline Acc merge(const Acc& a, const Acc& b) {
    std::vector<int> sizes = std::get<1>(a);
    for (int s : std::get<1>(b)) sizes.push_back(s);
    return Acc(std::get<0>(a) + std::get<0>(b), sizes);
}

inline std::string showAcc(const Acc& acc) {
    std::string sizes;
    for (size_t i = 0; i < std::get<1>(acc).size(); i++) {
        if (i > 0) sizes += ",";
        sizes += std::to_string(std::get<1>(acc)[i]);
    }
    return "sum=" + std::to_string(std::get<0>(acc)) + " sizes=" + sizes + "\n";
}

#endif
