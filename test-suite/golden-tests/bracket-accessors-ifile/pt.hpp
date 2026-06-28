#ifndef MORLOC_BRACKET_ACCESSORS_PT_HPP
#define MORLOC_BRACKET_ACCESSORS_PT_HPP

#include <vector>
#include <tuple>

// Field-order matches the morloc 'record Pt where x :: Int; y :: Int'
// declaration. Aggregate-init-friendly so the compiler can emit pt_t{x, y}
// directly from the record literal {x = ..., y = ...}.
struct pt_t {
    int x;
    int y;
};

// Field-order matches 'record Row where a :: [(Int, Int)]; b :: [Pt]'.
struct row_t {
    std::vector<std::tuple<int, int>> a;
    std::vector<pt_t> b;
};

// A no-op function so 'source Cpp from "pt.hpp"' has something to bind
// and the header is included in the generated C++ pool. Returns its
// argument unchanged.
template <class T>
inline T pt_passthrough(T x) { return x; }

#endif
