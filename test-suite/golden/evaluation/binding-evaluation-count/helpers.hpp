#include <cstdint>
#include <functional>
#include <vector>

template <class F, class A>
auto cpp_map(F f, const std::vector<A>& xs) {
    std::vector<decltype(f(xs[0]))> ys;
    for (const auto& x : xs) ys.push_back(f(x));
    return ys;
}

inline int cpp_id(int x) { return x; }

inline int cpp_val(int n) { return 2; }
