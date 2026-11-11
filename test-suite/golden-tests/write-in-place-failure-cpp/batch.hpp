#pragma once
#include <string>
#include <vector>

// n strings tagged with batch b; element `bad` (if in range) is not UTF-8.
std::vector<std::string> batchOf(int b, int n, int bad) {
    std::vector<std::string> xs;
    for (int i = 0; i < n; i++) {
        if (i == bad) {
            xs.push_back(std::string("bad\xff", 4));
        } else {
            xs.push_back(std::to_string(b) + "-" + std::to_string(i) + std::string(40, 'x'));
        }
    }
    return xs;
}

int countTagged(int b, const std::vector<std::string>& xs) {
    std::string tag = std::to_string(b) + "-";
    int n = 0;
    for (const auto& x : xs) {
        if (x.compare(0, tag.size(), tag) == 0) n++;
    }
    return n;
}
