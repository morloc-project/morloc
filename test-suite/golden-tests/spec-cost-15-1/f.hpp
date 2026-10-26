#include <vector>
inline int cpp_f(int x) { return 2 * x; }
template <class F> std::vector<int> cpp_map(F f, const std::vector<int>& xs) { std::vector<int> ys; for (int x : xs) ys.push_back(f(x)); return ys; }
inline int cpp_sum(const std::vector<int>& xs) { int s = 0; for (int x : xs) s += x; return s; }
