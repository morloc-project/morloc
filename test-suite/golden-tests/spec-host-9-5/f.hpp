#include <functional>
#include <tuple>
#include <vector>
inline int cpp_f(int x) { return 2 * x; }
template <class F> int cpp_apply(F f, int x) { return f(x); }
inline std::function<int(int)> cpp_add(int k) { return [k](int x) { return x + k; }; }
inline std::function<int(int)> cpp_make_adder(int k) { return [k](int x) { return x + k; }; }
inline std::tuple<std::function<int(int)>, int> cpp_pair(int k) {
    return std::make_tuple(std::function<int(int)>([k](int x) { return x * k; }), k);
}
template <class F> std::vector<int> cpp_map(F f, const std::vector<int>& xs) { std::vector<int> ys; for (int x : xs) ys.push_back(f(x)); return ys; }
inline std::vector<int> cpp_range(int n) { std::vector<int> xs; for (int i = 1; i <= n; i++) xs.push_back(i); return xs; }
inline int cpp_sum(const std::vector<int>& xs) { int s = 0; for (int x : xs) s += x; return s; }
