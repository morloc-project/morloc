#include <functional>
#include <iostream>
#include <vector>
inline int cpp_f(int x) { return 2 * x; }
inline std::function<int(int)> cpp_make_adder(int k) {
    std::cerr << "made in cpp" << std::endl;
    return [k](int x) { return x + k; };
}
template <class F> std::vector<int> cpp_map(F f, const std::vector<int>& xs) { std::vector<int> ys; for (int x : xs) ys.push_back(f(x)); return ys; }
inline int cpp_sum(const std::vector<int>& xs) { int s = 0; for (int x : xs) s += x; return s; }
inline int cpp_unrelated(int x) { return x; }
