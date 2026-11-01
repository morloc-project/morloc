#include <string>

std::string makeStr(int n) {
    return std::string(n, 'x');
}

int strLen(const std::string& s) {
    return static_cast<int>(s.size());
}
