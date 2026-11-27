#ifndef MORLOC_OPTIONAL_POLY_SAFEHEAD_HPP
#define MORLOC_OPTIONAL_POLY_SAFEHEAD_HPP

#include <optional>
#include <vector>

template <typename A>
bool list_is_empty(const std::vector<A>& xs) {
    return xs.empty();
}

template <typename A>
A list_at(int i, const std::vector<A>& xs) {
    return xs[i];
}

// Single templated overload. Intentionally no `std::nullopt_t`
// fallback so a bare `std::nullopt` argument from the morloc
// codegen would fail template-argument deduction. The test relies
// on CppPrinter emitting the typed `std::optional<A>{}` form for
// Null literals.
template <typename A>
A unwrap_or(A def, std::optional<A> x) {
    return x.has_value() ? *x : def;
}

#endif
