#pragma once
// Monomorphic projections of a @streamLayout result, used by the golden test.
// Sourcing these (rather than the stdlib Functor `map`) keeps the test on a
// single C++ pool and exercises the `_mlc_stream_layout` pool wrapper +
// from_voidstar<std::vector<std::tuple<...>>> path directly.
#include <cstdint>
#include <tuple>
#include <vector>

inline std::vector<uint64_t>
projOffsets(std::vector<std::tuple<uint64_t, uint64_t, uint64_t>> xs) {
    std::vector<uint64_t> out;
    out.reserve(xs.size());
    for (const auto& t : xs) out.push_back(std::get<0>(t));
    return out;
}

inline std::vector<uint64_t>
projCounts(std::vector<std::tuple<uint64_t, uint64_t, uint64_t>> xs) {
    std::vector<uint64_t> out;
    out.reserve(xs.size());
    for (const auto& t : xs) out.push_back(std::get<1>(t));
    return out;
}

inline int64_t
chunkCount(std::vector<std::tuple<uint64_t, uint64_t, uint64_t>> xs) {
    return static_cast<int64_t>(xs.size());
}
