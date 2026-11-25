#pragma once
#include <functional>
#include <stdexcept>
#include <thread>
#include <exception>

inline int boom(int x) {
    if (x == 0) throw std::runtime_error("boom");
    return x;
}

// Runs the suspension and discards its failure.
inline int attempt(std::function<int()> thunk) {
    try { return thunk(); } catch (...) { return -1; }
}

// Runs the suspension on a thread of its own and raises its failure here.
inline int on_thread(std::function<int()> thunk) {
    int out = 0;
    std::exception_ptr err;
    std::thread t([&] { try { out = thunk(); } catch (...) { err = std::current_exception(); } });
    t.join();
    if (err) std::rethrow_exception(err);
    return out;
}
