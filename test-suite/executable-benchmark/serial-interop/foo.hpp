#include <iostream>
#include <string>

template <class T>
T cid(T x) {
  return x;
}

template <class T>
int noop(const T& x) { return 1; }

int strlen(const std::string& x){
  return static_cast<int>(x.size());
}

std::string nmb(int n) {
    std::string result(1024 * 1024 * n, 'x');
    return result;
}
