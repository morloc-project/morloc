#include <functional>
#include <vector>

inline std::vector<int> scale(int k, const std::vector<int>& xs) {
    std::vector<int> ys;
    for (int x : xs) ys.push_back(k * x);
    return ys;
}

inline std::function<std::vector<int>(const std::vector<int>&)> scale_r(int k) {
    return [k](const std::vector<int>& xs) { return scale(k, xs); };
}

inline int mk3(int a, int b, int c) { return a * 100 + b * 10 + c; }

inline std::function<int(int, int)> mk_r(int a) {
    return [a](int b, int c) { return 1000 + a * 100 + b * 10 + c; };
}
