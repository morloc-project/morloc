#ifndef MORLOC_SPEC_OPT_7_1_HPP
#define MORLOC_SPEC_OPT_7_1_HPP

#include <optional>

template <typename A>
bool is_null(const std::optional<A>& x) {
    return !x.has_value();
}

#endif
