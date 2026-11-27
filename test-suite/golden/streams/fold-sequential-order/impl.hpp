#pragma once
#include <string>
#include <vector>

std::string empty_str() { return std::string(); }

std::string cat_batch(const std::string& acc, const std::vector<std::string>& xs) {
    std::string out = acc;
    for (const auto& x : xs) out += x;
    return out;
}

std::string cat(const std::string& a, const std::string& b) { return a + b; }

std::string show_s(const std::string& s) { return s; }
