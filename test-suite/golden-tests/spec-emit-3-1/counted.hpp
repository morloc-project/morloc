#pragma once
#include <vector>

// Counts every copy and move of a Counted, so a test sees what the
// generated code does with a value it returns.
struct Counted {
    int v;
    static int* tally() { static int t[2] = {0, 0}; return t; }
    Counted() : v(0) {}
    explicit Counted(int x) : v(x) {}
    Counted(const Counted& o) : v(o.v) { tally()[0]++; }
    Counted(Counted&& o) noexcept : v(o.v) { tally()[1]++; }
    Counted& operator=(const Counted& o) { v = o.v; tally()[0]++; return *this; }
    Counted& operator=(Counted&& o) noexcept { v = o.v; tally()[1]++; return *this; }
};

inline Counted make(int x) { return Counted(x); }
inline Counted bump(const Counted& c) { return Counted(c.v + 1); }
inline std::vector<int> tally(const Counted& c) {
    return {c.v, Counted::tally()[0], Counted::tally()[1]};
}
inline Counted pack_counted(int x) { return Counted(x); }
inline int unpack_counted(const Counted& c) { return c.v; }
