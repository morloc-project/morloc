#ifndef MORLOC_OPTIONAL_TEST_HPP
#define MORLOC_OPTIONAL_TEST_HPP

#include <optional>
#include <vector>
#include <string>

template <typename T>
bool isNull(const std::optional<T>& x) {
    return !x.has_value();
}

template <typename T>
T fromNull(const T& default_val, const std::optional<T>& x) {
    if (!x.has_value()) return default_val;
    return *x;
}

template <typename T>
T fromNull(const T& default_val, std::nullopt_t) {
    return default_val;
}

template <typename T>
std::optional<T> toNull(const T& x) {
    return std::optional<T>(x);
}

std::optional<int> safeHead(const std::vector<int>& xs) {
    if (xs.empty()) return std::nullopt;
    return std::optional<int>(xs[0]);
}

std::optional<int> optionalAdd(const std::optional<int>& x, const std::optional<int>& y) {
    if (!x.has_value() || !y.has_value()) return std::nullopt;
    return std::optional<int>(*x + *y);
}

std::vector<std::optional<int>> optionalList(const std::vector<int>& xs) {
    std::vector<std::optional<int>> result;
    result.reserve(xs.size());
    for (const auto& x : xs) {
        if (x < 0) result.push_back(std::nullopt);
        else result.push_back(std::optional<int>(x));
    }
    return result;
}

int countNulls(const std::vector<std::optional<int>>& xs) {
    int count = 0;
    for (const auto& x : xs) {
        if (!x.has_value()) count++;
    }
    return count;
}

#endif
