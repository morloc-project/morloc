#ifndef __HELPERS_HPP__
#define __HELPERS_HPP__

#include <cstdint>
#include <vector>

inline int64_t& cpp_tally() { static int64_t v = 0; return v; }

inline int  cpp_add_one(int x)     { return x + 1; }
inline int  cpp_read()             { return 10; }
inline bool cpp_gate(int x)        { return x > 0; }
inline int  cpp_total()            { return static_cast<int>(cpp_tally()); }
inline int  cpp_multi_err(int x)   { return x * 2; }
inline int  cpp_nullary()          { return 3; }

inline void cpp_tap(int x)         { cpp_tally() += static_cast<int64_t>(x); }
inline void cpp_reset_tally()      { cpp_tally() = 0; }

template <class Sink>
inline void cpp_for_each(const std::vector<int>& xs, Sink sink) {
    for (int x : xs) sink(x);
}

#endif
