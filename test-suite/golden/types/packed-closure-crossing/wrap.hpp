#pragma once
#include <functional>

struct MyWrapC { int v; };

inline MyWrapC pack_wrap_cpp(int x) { return MyWrapC{x}; }
inline int unpack_wrap_cpp(const MyWrapC& w) { return w.v; }

inline MyWrapC mk_wrapped_cpp(int x) { return MyWrapC{x}; }

inline int apply_wrapped_cpp(std::function<int(MyWrapC)> f, MyWrapC w) {
    return f(w);
}

inline int apply_mk_cpp(std::function<MyWrapC(int)> f, int x) {
    return f(x).v;
}
