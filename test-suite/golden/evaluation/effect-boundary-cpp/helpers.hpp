#ifndef __HELPERS_HPP__
#define __HELPERS_HPP__

#include <cstdint>
#include <vector>

// Mutable tally observed by 'totalSoFar'. Any callback failing to fire
// its effect leaves the tally at its previous value.
inline int64_t& tally() { static int64_t v = 0; return v; }

// Sources with primitive-returning shapes.
inline int  gen_small()      { return 1; }
inline int  add_one(int x)   { return x + 1; }
inline int  risky(int x)     { return x * 2; }
inline int  read_value()     { return 10; }
inline int  total_so_far()   { return static_cast<int>(tally()); }

// Void-returning sources (declared '<E> ()' morloc-side). These are the
// void->Unit adapter test cases: 'makeLet' must emit
// @poke(x); mlc::Unit ni{};@ rather than @mlc::Unit ni = poke(x)@.
inline void tap(int x)       { tally() += static_cast<int64_t>(x); }
inline void reset_tally()    { tally() = 0; }
inline void poke(int x)      { tally() += static_cast<int64_t>(x); }

// Template callback consumer: invokes @sink(x)@ and discards. A
// callback returning 'std::function<Unit()>' (unforced thunk) would
// leave the tally unchanged.
template <class Sink>
inline void for_each(const std::vector<int>& xs, Sink sink) {
    for (int x : xs) sink(x);
}

#endif
