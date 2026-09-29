#ifndef STREAM_WRITE_BEHIND_HPP
#define STREAM_WRITE_BEHIND_HPP

#include <cstdint>
#include <string>
#include <vector>

// Batch `b` of a stream of `per` strings. Lengths cycle through 3..27, so
// most elements need alignment padding in the write buffer.
inline std::vector<std::string> wbBatch(int b, int per) {
    std::vector<std::string> out;
    for (int i = 0; i < per; i++) {
        int k = b * per + i;
        std::string s = std::to_string(k) + ":";
        s.append(1 + k % 23, static_cast<char>('a' + k % 26));
        out.push_back(s);
    }
    return out;
}

template <class F>
void wbProduce(int n, int per, F sink) {
    for (int b = 0; b < n; b++) sink(wbBatch(b, per));
}

// An order-sensitive digest of a list of strings (FNV-1a over each string
// and a separator).
inline int64_t wbDigest(const std::vector<std::string>& xs) {
    uint64_t h = 1469598103934665603ULL;
    for (const auto& s : xs) {
        for (unsigned char c : s) { h ^= c; h *= 1099511628211ULL; }
        h ^= 0xff; h *= 1099511628211ULL;
    }
    return static_cast<int64_t>(h >> 1);
}

// The digest `wbProduce n per` should leave behind.
inline int64_t wbExpected(int n, int per) {
    std::vector<std::string> all;
    for (int b = 0; b < n; b++) {
        auto xs = wbBatch(b, per);
        all.insert(all.end(), xs.begin(), xs.end());
    }
    return wbDigest(all);
}

#endif
