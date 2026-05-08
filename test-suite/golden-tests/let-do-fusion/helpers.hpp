#ifndef __HELPERS_HPP__
#define __HELPERS_HPP__

#include <iostream>

// Side-effecting functions: write a tag to stderr, return a deterministic value.
// The tag lets us verify the ORDER of evaluation; the returned value lets us
// verify CORRECTNESS of computation.
inline int cppEcho(int x) {
    std::cerr << "C" << x << std::endl;
    return x + 1;
}

inline int cppAdd(int a, int b) {
    return a + b;
}

inline int cppMul(int a, int b) {
    return a * b;
}

inline int cppPure(int x) {
    return x + 10;
}

inline int cppId(int x) {
    return x;
}

// Stateful counter: stable across calls within ONE C++ pool. Used to verify
// that side-effects are not duplicated (no double-evaluation of let bindings).
inline int cppNextN() {
    static int n = 0;
    return ++n;
}

inline void cppResetCounter() {
    static int* p = []() {
        // no-op; counter cannot truly reset across calls but each pool restart resets
        return new int(0);
    }();
    (void)p;
}

#endif
