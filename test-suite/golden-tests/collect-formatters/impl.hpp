#pragma once
#include <vector>
#include <string>
#include <cstdint>
#include <algorithm>

// Per-batch integer data. Batch i is three ints, deliberately unsorted so
// sort_ints has visible effect: {i*10+3, i*10+1, i*10+2}.
inline std::vector<int> make_ints(int i) {
    return { i * 10 + 3, i * 10 + 1, i * 10 + 2 };
}

// whole-list: sort the gathered ints
inline std::vector<int> sort_ints(std::vector<int> xs) {
    std::sort(xs.begin(), xs.end());
    return xs;
}

// per-batch: append the batch length as a trailing marker
inline std::vector<int> tag_ints(std::vector<int> xs) {
    xs.push_back(static_cast<int>(xs.size()));
    return xs;
}

// per-batch + offset: prepend the running element offset so continuity across
// batches is visible in the output (0, then 3, then 6 for 3-element batches)
inline std::vector<int> tag_ints_off(uint64_t off, std::vector<int> xs) {
    xs.insert(xs.begin(), static_cast<int>(off));
    return xs;
}

// per-batch render to one text line
inline std::string render_ints(std::vector<int> xs) {
    std::string s;
    for (int x : xs) { s += std::to_string(x); s += " "; }
    s += "\n";
    return s;
}

// per-batch render with offset prefix
inline std::string render_ints_off(uint64_t off, std::vector<int> xs) {
    std::string s = "[" + std::to_string(off) + "]";
    for (int x : xs) { s += " "; s += std::to_string(x); }
    s += "\n";
    return s;
}

// Per-batch string data. Batch i is two strings, unsorted.
inline std::vector<std::string> make_strs(int i) {
    return { std::string("b") + std::to_string(i), std::string("a") + std::to_string(i) };
}

inline std::vector<std::string> sort_strs(std::vector<std::string> xs) {
    std::sort(xs.begin(), xs.end());
    return xs;
}

inline std::string join_strs(std::vector<std::string> xs) {
    std::string s;
    for (const std::string& x : xs) { s += x; s += ";"; }
    s += "\n";
    return s;
}
