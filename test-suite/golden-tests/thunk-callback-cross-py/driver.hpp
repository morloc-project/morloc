#ifndef __DRIVER_HPP__
#define __DRIVER_HPP__

#include <vector>

// Iterates the list and calls the callback with each element. The
// callback is a std::function-like sink; if it fails to fire its
// effect on invocation (returns an unforced thunk that we then
// discard), no side effect reaches the Python-side tally.
template <class Sink>
inline void forEach(const std::vector<int>& xs, Sink sink) {
    for (int x : xs) sink(x);
}

#endif
