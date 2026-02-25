#ifndef MORLOC_OPTIONAL_COERCE_TEST_HPP
#define MORLOC_OPTIONAL_COERCE_TEST_HPP

#include <optional>

template <typename T>
T fromNull(const T& default_val, const std::optional<T>& x) {
    if (!x.has_value()) return default_val;
    return *x;
}

std::optional<int> addOpt(const std::optional<int>& x, const std::optional<int>& y) {
    if (!x.has_value() || !y.has_value()) return std::nullopt;
    return std::optional<int>(*x + *y);
}

std::optional<int> identity(const std::optional<int>& x) {
    return x;
}

#endif
