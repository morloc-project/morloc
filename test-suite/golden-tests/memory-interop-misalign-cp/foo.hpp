#ifndef MORLOC_MEMORY_INTEROP_MISALIGN_HPP
#define MORLOC_MEMORY_INTEROP_MISALIGN_HPP

#include <optional>

struct pair_br_t {
    bool flag;
    double value;
};

std::optional<double> cMakeOptReal(double x) {
    return std::optional<double>(x);
}

double cFromNullReal(double def, const std::optional<double>& x) {
    if (!x.has_value()) return def;
    return *x;
}

std::optional<double> cDoubleOptReal(const std::optional<double>& x) {
    if (!x.has_value()) return std::nullopt;
    return std::optional<double>(*x * 2.0);
}

pair_br_t cMakePair(bool flag, double value) {
    return pair_br_t{flag, value};
}

double cGetPairValue(const pair_br_t& p) {
    return p.value;
}

#endif
