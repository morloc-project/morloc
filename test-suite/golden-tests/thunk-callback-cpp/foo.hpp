#ifndef __FOO_HPP__
#define __FOO_HPP__

#include <cstdint>
#include <vector>

// Static accumulator observed via `totalSoFar`. Each `tap` call
// mutates it; if the callback fails to fire its effect on
// invocation (the manifold returns std::function<Unit()> that the
// driver drops), the tally stays at its previous value.
inline int64_t& tally() { static int64_t v = 0; return v; }

inline void tap(int x)      { tally() += static_cast<int64_t>(x); }
inline int totalSoFar()     { return static_cast<int>(tally()); }
inline void resetTally()    { tally() = 0; }

// Iterates the list and calls the callback with each element. Any
// callback that silently discards its thunk leaves the tally at 0.
template <class Sink>
inline void forEach(const std::vector<int>& xs, Sink sink) {
    for (int x : xs) sink(x);
}

#endif
