#pragma once
#include <cstdint>
#include <string>
#include <vector>

struct hit_t {
    std::string name;
    uint64_t score;
};

template <typename F>
void produce_rows(int64_t b, F sink) {
    for (int64_t i = 0; i < b; i++) {
        std::vector<std::vector<uint64_t>> batch;
        for (int64_t j = 0; j < i; j++) batch.push_back({(uint64_t)(i * 10 + j), (uint64_t)j});
        sink(batch);
    }
}

template <typename F>
void produce_strs(int64_t b, F sink) {
    for (int64_t i = 0; i < b; i++) {
        std::vector<std::string> batch;
        for (int64_t j = 0; j < i; j++) batch.push_back("s" + std::to_string(i) + " " + std::to_string(j));
        if (i == 1) batch.push_back("");
        sink(batch);
    }
}

template <typename F>
void produce_hits(int64_t b, F sink) {
    for (int64_t i = 0; i < b; i++) {
        std::vector<hit_t> batch;
        for (int64_t j = 0; j < i; j++) batch.push_back({"h" + std::to_string(i) + std::to_string(j), (uint64_t)(i * 10 + j)});
        sink(batch);
    }
}

inline std::string show_rows(const std::vector<std::vector<uint64_t>>& xs) {
    std::string out = "rows " + std::to_string(xs.size()) + ":";
    for (auto& r : xs) {
        out += " ";
        for (size_t k = 0; k < r.size(); k++) out += (k ? "," : "") + std::to_string(r[k]);
    }
    return out + "\n";
}

inline std::string show_strs(const std::vector<std::string>& xs) {
    std::string out = "strs " + std::to_string(xs.size()) + ":";
    for (auto& s : xs) out += " [" + s + "]";
    return out + "\n";
}

inline std::string show_hits(const std::vector<hit_t>& xs) {
    std::string out = "hits " + std::to_string(xs.size()) + ":";
    for (auto& h : xs) out += " " + h.name + "=" + std::to_string(h.score);
    return out + "\n";
}
