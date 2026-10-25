#include <vector>

template <class F, class A, class B>
B left_fold(F f, B b, const std::vector<A>& xs) {
    for (const auto& x : xs) b = f(b, x);
    return b;
}

inline int plus(int a, int b) { return a + b; }
