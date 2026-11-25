#pragma once
#include <csignal>

// A fault the kernel raises.
inline int crash(int x) {
    volatile int* p = nullptr;
    *p = x;
    return x;
}

// A signal the process sends itself.
inline int crash_raised(int x) {
    std::raise(SIGSEGV);
    return x;
}
