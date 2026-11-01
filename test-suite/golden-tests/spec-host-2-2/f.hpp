#include <functional>

template <class K>
int apply2(K k, int a, int b) { return k(a, b); }

template <class K>
int feed(K k) { return k(std::function<int(int, int)>([](int a, int b) { return a - b; })); }

inline std::function<int(int, int)> make_sub(int n) {
    return [n](int a, int b) { return (a - b) * n; };
}

#include <vector>

template <class F>
auto first2(const std::vector<F>& fs, int x) { return fs[0](x, x); }

template <class K>
auto feed_split(K k) {
    return k(std::function<std::function<int(int)>(int)>([](int a) {
        return std::function<int(int)>([a](int y) { return a * 10 + y; });
    }));
}
