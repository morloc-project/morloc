#ifndef __IMPL_HPP__
#define __IMPL_HPP__

#include <string>
#include <thread>
#include <tuple>
#include <vector>

// One batch of consecutive integers, sized by its index: 1, 2, 3, ...
inline std::vector<int> mk(int i) {
    std::vector<int> out;
    int start = i * (i + 1) / 2;
    for (int k = 0; k <= i; k++) out.push_back(start + k);
    return out;
}

// Drive the sink from four real threads, one batch each. Each thread folds
// into its own accumulator, so the merge has four to combine.
template <typename F>
void produceParallel(F sink) {
    std::vector<std::thread> ts;
    for (int i = 0; i < 4; i++) {
        ts.emplace_back([&sink, i]() { sink(mk(i)); });
    }
    for (auto& t : ts) { t.join(); }
}

// (running total, batches folded, merges performed).
//
// Every part is commutative, so the answer does not depend on the order the
// threads happened to finish in. The `merges` counter is deliberately NOT
// identity-respecting -- `mergePAcc` increments it, so the seed is not an
// identity for it -- because counting the merges is the only way to observe
// that `combine` ran at all. It stays deterministic because the producer
// starts exactly four threads, so there are exactly four accumulators.
// An ordinary accumulator must be a monoid; this one is an instrument.
using PAcc = std::tuple<int, int, int>;

inline PAcc zeroPAcc() { return PAcc(0, 0, 0); }

inline PAcc addPBatch(const PAcc& acc, const std::vector<int>& batch) {
    int sum = std::get<0>(acc);
    for (int x : batch) sum += x;
    return PAcc(sum, std::get<1>(acc) + 1, std::get<2>(acc));
}

inline PAcc mergePAcc(const PAcc& a, const PAcc& b) {
    return PAcc(std::get<0>(a) + std::get<0>(b),
                std::get<1>(a) + std::get<1>(b),
                std::get<2>(a) + std::get<2>(b) + 1);
}

inline std::string showPAcc(const PAcc& acc) {
    return "sum=" + std::to_string(std::get<0>(acc))
         + " batches=" + std::to_string(std::get<1>(acc))
         + " merges=" + std::to_string(std::get<2>(acc)) + "\n";
}

using Acc = std::tuple<int, std::vector<int>>;

inline Acc zeroAcc() { return Acc(0, std::vector<int>()); }

// The accumulator records the batch sizes it was fed, not just the running
// total, so a lowering that flattens the stream into one batch answers
// differently from one that keeps the producer's boundaries.
inline Acc addBatch(const Acc& acc, const std::vector<int>& batch) {
    int sum = std::get<0>(acc);
    for (int x : batch) sum += x;
    std::vector<int> sizes = std::get<1>(acc);
    sizes.push_back((int)batch.size());
    return Acc(sum, sizes);
}

inline Acc mergeAcc(const Acc& a, const Acc& b) {
    std::vector<int> sizes = std::get<1>(a);
    for (int s : std::get<1>(b)) sizes.push_back(s);
    return Acc(std::get<0>(a) + std::get<0>(b), sizes);
}

inline std::string showAcc(const Acc& acc) {
    std::string out = "sum=" + std::to_string(std::get<0>(acc)) + " batches=";
    const std::vector<int>& sizes = std::get<1>(acc);
    for (size_t i = 0; i < sizes.size(); i++) {
        if (i) out += ",";
        out += std::to_string(sizes[i]);
    }
    return out + "\n";
}

inline std::string joinAll(const std::vector<int>& xs) {
    std::string out;
    for (size_t i = 0; i < xs.size(); i++) {
        if (i) out += ",";
        out += std::to_string(xs[i]);
    }
    return out + "\n";
}

#endif
