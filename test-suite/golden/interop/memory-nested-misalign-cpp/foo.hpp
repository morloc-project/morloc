#ifndef MORLOC_MEMORY_NESTED_MISALIGN_HPP
#define MORLOC_MEMORY_NESTED_MISALIGN_HPP

#include <optional>
#include <vector>

struct pair_br_t {
    bool flag;
    double value;
};

struct record_with_opt_t {
    bool flag;
    std::optional<double> opt;
};

template <typename T>
std::optional<T> toNull(const T& x) {
    return std::optional<T>(x);
}

std::optional<pair_br_t> makeOptRecord(bool flag, double value) {
    return std::optional<pair_br_t>(pair_br_t{flag, value});
}

pair_br_t fromNullRecord(const pair_br_t& def, const std::optional<pair_br_t>& x) {
    if (!x.has_value()) return def;
    return *x;
}

double sumRecordValues(const std::vector<pair_br_t>& records) {
    double sum = 0.0;
    for (const auto& r : records) {
        sum += r.value;
    }
    return sum;
}

record_with_opt_t makeRecordWithOpt(bool flag, const std::optional<double>& opt) {
    return record_with_opt_t{flag, opt};
}

std::optional<double> getRecordOpt(const record_with_opt_t& r) {
    return r.opt;
}

std::optional<pair_br_t> nestedRoundTrip(const std::optional<pair_br_t>& x) {
    if (!x.has_value()) return std::nullopt;
    return std::optional<pair_br_t>(pair_br_t{x->flag, x->value});
}

#endif
