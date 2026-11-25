#ifndef MORLOC_OPTIONAL_INTEROP_RC_HPP
#define MORLOC_OPTIONAL_INTEROP_RC_HPP

#include <optional>

std::optional<int> cSafeDiv(int x, int y) {
    if (y == 0) return std::nullopt;
    return std::optional<int>(x / y);
}

int cFromNull(int default_val, const std::optional<int>& x) {
    if (!x.has_value()) return default_val;
    return *x;
}

std::optional<int> cDouble(const std::optional<int>& x) {
    if (!x.has_value()) return std::nullopt;
    return std::optional<int>(*x * 2);
}

#endif
