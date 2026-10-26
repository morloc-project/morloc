#include <vector>

template <class F, class A>
auto cpp_map(F f, const std::vector<A>& xs) {
    std::vector<decltype(f(xs[0]))> ys;
    for (const auto& x : xs) ys.push_back(f(x));
    return ys;
}

inline std::vector<int> upto(int n) {
    std::vector<int> xs;
    for (int i = 1; i <= n; i++) xs.push_back(i);
    return xs;
}

inline int total_of(const std::vector<int>& xs) {
    int s = 0;
    for (int x : xs) s += x;
    return s;
}
