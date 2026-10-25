#include <vector>

inline std::vector<int> incr(const std::vector<int>& xs) {
    std::vector<int> ys;
    ys.reserve(xs.size());
    for (int x : xs) ys.push_back(x + 1);
    return ys;
}
