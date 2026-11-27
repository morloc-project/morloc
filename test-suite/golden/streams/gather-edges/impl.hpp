#pragma once
#include <cstdint>
#include <fstream>
#include <functional>
#include <stdexcept>
#include <string>
#include <vector>

template <typename F>
void produce_none(F sink) {
    (void)sink;
}

template <typename F>
void produce_fail(F sink) {
    sink(std::vector<int64_t>{1, 2});
    sink(std::vector<int64_t>{3});
    throw std::runtime_error("producer failed after 2 batches");
}

template <typename F>
void produce_ten(F sink) {
    sink(std::vector<int64_t>{0, 1, 2});
    sink(std::vector<int64_t>{});
    sink(std::vector<int64_t>{3, 4, 5, 6, 7, 8, 9});
}

inline std::string show_ints(const std::vector<int64_t>& xs) {
    std::string out = "cpp " + std::to_string(xs.size()) + ":";
    for (auto x : xs) out += " " + std::to_string(x);
    return out + "\n";
}

inline std::string mark_ran(const std::vector<int64_t>& xs) {
    std::ofstream("handler-ran") << xs.size() << "\n";
    return "handler ran\n";
}

inline std::function<void(const std::vector<int64_t>&)>& stash_slot() {
    static std::function<void(const std::vector<int64_t>&)> f;
    return f;
}

template <typename F>
void produce_stash(F sink) {
    sink(std::vector<int64_t>{1, 2});
    stash_slot() = [sink](const std::vector<int64_t>& xs) { sink(xs); };
}

inline std::string show_late(const std::vector<int64_t>& xs) {
    if (stash_slot()) stash_slot()(std::vector<int64_t>{3});
    return show_ints(xs);
}
