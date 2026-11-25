#include <cstdlib>
#include <fstream>
#include <string>
#include <vector>

inline std::string tick_log() { return std::getenv("TICK_LOG"); }

template <class T>
T tick(T x) {
    std::ofstream f(tick_log(), std::ios::app);
    f << "tick\n";
    return x;
}

inline int count_ticks() {
    std::ifstream f(tick_log());
    std::string line;
    int n = 0;
    while (std::getline(f, line)) n++;
    return n;
}

template <class T>
int ticks(T) { return count_ticks(); }

template <class T>
int ticks_l(const std::vector<T>&) { return count_ticks(); }

inline int cpp_add(int a, int b) { return a + b; }

template <class F, class A>
auto host_map(F f, const std::vector<A>& xs) {
    std::vector<decltype(f(xs[0]))> ys;
    for (const auto& x : xs) ys.push_back(f(x));
    return ys;
}

template <class F, class A, class B>
auto host_map2(F f, const std::vector<A>& xs, const std::vector<B>& ys) {
    std::vector<decltype(f(xs[0], ys[0]))> zs;
    for (size_t i = 0; i < xs.size() && i < ys.size(); i++) zs.push_back(f(xs[i], ys[i]));
    return zs;
}
