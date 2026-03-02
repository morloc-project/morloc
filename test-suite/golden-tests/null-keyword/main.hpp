#ifndef MORLOC_NULL_KEYWORD_TEST_HPP
#define MORLOC_NULL_KEYWORD_TEST_HPP

#include <optional>

template <typename T>
T fromNull(const T& default_val, const std::optional<T>& x) {
    if (!x.has_value()) return default_val;
    return *x;
}

#endif
