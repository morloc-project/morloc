#ifndef __HELPERS_HPP__
#define __HELPERS_HPP__

#include <cstdint>
#include <functional>
#include <optional>
#include <vector>

inline std::optional<int> cpp_read_opt(int x) {
    if (x < 0) return std::nullopt;
    return x;
}

inline int cpp_add_one(int x) {
    return x + 1;
}

inline int cpp_maybe_error(int x) {
    if (x < 0) throw std::runtime_error("cpp_maybe_error: negative");
    return x * 2;
}

inline int cpp_pure_add(int x, int y) {
    return x + y;
}

// Consume a list of nullary-callable thunks; invoke each and sum.
inline int cpp_sum_thunks(const std::vector<std::function<int()>>& v) {
    int s = 0;
    for (auto const& f : v) s += f();
    return s;
}

// fromNull: if @m holds a value, return it; else return the default.
inline int cpp_from_null(int deflt, std::optional<int> m) {
    return m ? *m : deflt;
}

// Take a nullary callable, force it, return value + 1.
inline int cpp_apply_thunk(std::function<int()> f) {
    return f() + 1;
}

// Two-slot tuple of nullary callables; force both, sum.
inline int cpp_consume_tuple(std::tuple<std::function<int()>, std::function<int()>> t) {
    return std::get<0>(t)() + std::get<1>(t)();
}

#endif
