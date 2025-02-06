#include <stack>
#include <list>
#include <forward_list>
#include <deque>
#include <queue>
#include <vector>
#include <algorithm>


// Stack to Vector and back
template <typename T>
std::vector<T> toVector(const std::stack<T>& xs) {
    std::vector<T> result;
    std::stack<T> temp = xs;  // Create a temporary stack to preserve the original
    result.reserve(temp.size());
    while (!temp.empty()) {
        result.push_back(temp.top());
        temp.pop();
    }
    std::reverse(result.begin(), result.end());  // Reverse to maintain original order
    return result;
}

// List to Vector and back
template <typename T>
std::vector<T> toVector(const std::list<T>& xs) {
    return std::vector<T>(xs.begin(), xs.end());
}

// Forward List to Vector and back
template <typename T>
std::vector<T> toVector(const std::forward_list<T>& xs) {
    return std::vector<T>(xs.begin(), xs.end());
}

// Deque to Vector and back
template <typename T>
std::vector<T> toVector(const std::deque<T>& xs) {
    return std::vector<T>(xs.begin(), xs.end());
}

// Queue to Vector and back
template <typename T>
std::vector<T> toVector(std::queue<T> xs) {  // Note: pass by value to allow modification
    std::vector<T> result;
    result.reserve(xs.size());
    while (!xs.empty()) {
        result.push_back(std::move(xs.front()));
        xs.pop();
    }
    return result;
}




template <typename T>
std::stack<T> vectorToStack(const std::vector<T>& xs) {
    return std::stack<T>(std::deque<T>(xs.begin(), xs.end()));
}

template <typename T>
std::list<T> vectorToList(const std::vector<T>& xs) {
    return std::list<T>(xs.begin(), xs.end());
}

template <typename T>
std::forward_list<T> vectorToForwardList(const std::vector<T>& xs) {
    return std::forward_list<T>(xs.begin(), xs.end());
}

template <typename T>
std::deque<T> vectorToDeque(const std::vector<T>& xs) {
    return std::deque<T>(xs.begin(), xs.end());
}

template <typename T>
std::queue<T> vectorToQueue(const std::vector<T>& xs) {
    return std::queue<T>(std::deque<T>(xs.begin(), xs.end()));
}


template <typename T>
T& id(const T& x) {
    return x;
}

// Vector
template <typename T>
std::vector<T> append(T element, std::vector<T> container) {
    container.push_back(element);
    return container;
}

// List
template <typename T>
std::list<T> append(T element, std::list<T> container) {
    container.push_back(element);
    return container;
}

// Forward List - a horribly dodgy thing to do in a forward list
template <typename T>
std::forward_list<T> append(T element, std::forward_list<T> container) {
    if (container.empty()) {
        container.push_front(std::move(element));
    } else {
        auto it = container.before_begin();
        auto end = container.end();
        while (std::next(it) != end) {
            ++it;
        }
        container.insert_after(it, std::move(element));
    }
    return container;
}

// Deque
template <typename T>
std::deque<T> append(T element, std::deque<T> container) {
    container.push_back(element);
    return container;
}

// Stack
template <typename T>
std::stack<T> append(T element, std::stack<T> container) {
    container.push(element);
    return container;
}

// Queue
template <typename T>
std::queue<T> append(T element, std::queue<T> container) {
    container.push(element);
    return container;
}
