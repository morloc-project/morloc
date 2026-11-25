#ifndef MORLOC_MEMORY_OPTIONAL_DOUBLE_HPP
#define MORLOC_MEMORY_OPTIONAL_DOUBLE_HPP

#include <optional>

template <typename T>
std::optional<T> toNull(const T& x) {
    return std::optional<T>(x);
}

template <typename T>
bool isNull(const std::optional<T>& x) {
    return !x.has_value();
}

template <typename T>
T fromNull(const T& def, const std::optional<T>& x) {
    if (!x.has_value()) return def;
    return *x;
}

std::optional<double> doubleOptReal(const std::optional<double>& x) {
    if (!x.has_value()) return std::nullopt;
    return std::optional<double>(*x * 2.0);
}

std::optional<int> addOptInt(const std::optional<int>& x, const std::optional<int>& y) {
    if (!x.has_value() || !y.has_value()) return std::nullopt;
    return std::optional<int>(*x + *y);
}

#endif
