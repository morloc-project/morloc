#include <functional>

std::function<int(int)> bar(bool cond1, bool cond2) {
    if (cond1) {
        printf("cond1 %d\n", cond1);
        return [](int x) { return 2 * x; };
    } else {
        printf("cond1\n");
        return [cond2](int x) { return 3 * x + (int)cond2; };
    }
}

std::function<std::function<int(bool)>(int)> baz(bool cond1) {
    auto f1 = [cond1](int num) {
        auto f2 = [cond1, num](bool cond2) {
            return cond1 + num + 3 * (int)cond2;
        };
        return f2;
    };
    return f1;
}
