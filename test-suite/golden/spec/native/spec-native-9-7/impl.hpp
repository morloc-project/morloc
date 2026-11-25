#pragma once
#include <set>
#include <string>
#include <vector>
inline std::vector<int> mk(int i) { return {i, i + 10}; }
inline std::set<int> zeroAcc() { return {}; }
inline std::set<int> addBatch(const std::set<int>& a, const std::vector<int>& b) { std::set<int> r = a; r.insert(b.begin(), b.end()); return r; }
inline std::set<int> mergeAcc(const std::set<int>& a, const std::set<int>& b) { std::set<int> r = a; r.insert(b.begin(), b.end()); return r; }
inline std::string showAcc(const std::set<int>& a) { std::string s; for (int x : a) s += std::to_string(x) + " "; return s; }
#include <functional>
#include <thread>
inline void produceParallel(std::function<void(std::vector<int>)> sink) {
    std::vector<std::thread> ts;
    for (int t = 0; t < 4; t++) ts.emplace_back([&sink, t]() { sink({t, t + 100}); });
    for (auto& th : ts) th.join();
}
