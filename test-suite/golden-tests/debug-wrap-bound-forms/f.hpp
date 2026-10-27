#include <functional>

inline std::function<int(int)> cpp_make_adder(int k) { return [k](int x) { return x + k; }; }

template <class F>
int cpp_apply(F f, int x) { return f(x); }
