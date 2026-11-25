#include <functional>
#include <stdexcept>
int safeApply(std::function<int(int)> f, int x) {
    try { return f(x); } catch (const std::exception& e) { return -1; }
}
