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

// n lists tagged with batch b: every fifth list is empty, the others hold
// a tag, an empty Str, a two-byte character and 40 bytes. In list `at`, the
// piece `bad` is "ab\xffc" instead.
std::vector<std::vector<std::string>> piecesOf(int b, int n, int at, int bad) {
    std::vector<std::vector<std::string>> xss;
    for (int i = 0; i < n; i++) {
        std::vector<std::string> xs;
        if (i % 5 != 4) {
            xs = {std::to_string(b) + "-" + std::to_string(i), "", "\xc3\xa9", std::string(40, 'y')};
        }
        if (i == at) {
            xs.resize(4);
            xs[bad] = std::string("ab\xff" "c", 4);
        }
        xss.push_back(xs);
    }
    return xss;
}

// n lists tagged with batch b, each a tag and 300000 bytes; in list `at`,
// byte 299990 of the large piece is not UTF-8.
std::vector<std::vector<std::string>> bigOf(int b, int n, int at) {
    std::vector<std::vector<std::string>> xss;
    for (int i = 0; i < n; i++) {
        std::string big(300000, 'z');
        if (i == at) big[299990] = '\xff';
        xss.push_back({std::to_string(b) + "-" + std::to_string(i), big});
    }
    return xss;
}

int countPieces(int b, const std::vector<std::vector<std::string>>& xss) {
    std::string tag = std::to_string(b) + "-";
    int n = 0;
    for (const auto& xs : xss) {
        if (!xs.empty() && xs[0].compare(0, tag.size(), tag) == 0) n++;
    }
    return n;
}
