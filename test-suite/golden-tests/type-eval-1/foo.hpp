#include <stack>
#include <vector>

template <typename T>
std::vector<T> stackToVector(const std::stack<T>& xs) {
    std::vector<T> result;
    std::stack<T> temp = xs;  // Create a temporary stack to preserve the original
    while (!temp.empty()) {
        result.push_back(temp.top());
        temp.pop();
    }
    std::reverse(result.begin(), result.end());  // Reverse to maintain original order
    return result;
}

template <typename T>
std::stack<T> vectorToStack(const std::vector<T>& xs) {
    std::stack<T> result;
    for (const T& x : xs) {
        result.push(x);
    }
    return result;
}

template <typename T>
std::stack<T> push(T x, std::stack<T> xs){
    xs.push(x);
    return xs;
}
