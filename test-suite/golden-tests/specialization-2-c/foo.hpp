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
    std::stack<T> result;
    for (auto it = xs.rbegin(); it != xs.rend(); ++it) {
        result.push(*it);
    }
    return result;
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
T id(const T& x) {
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

// Deque
template <typename T>
std::deque<T> append(T element, std::deque<T> container) {
    container.push_back(element);
    return container;
}

// Queue - WARNING: O(n)
template <typename T>
std::queue<T> append(T element, std::queue<T> container) {
    std::queue<T> temp_queue;

    // Copy elements from original queue to temp_queue
    while (!container.empty()) {
        temp_queue.push(container.front());
        container.pop();
    }

    // Push the new element onto the end of the queue
    temp_queue.push(element);

    // Copy elements back from temp_queue to original queue
    while (!temp_queue.empty()) {
        container.push(temp_queue.front());
        temp_queue.pop();
    }

    return container;
}

// Forward List - WARNING: O(n)
template <typename T>
std::forward_list<T> append(T element, std::forward_list<T> container) {
    std::forward_list<T> new_container;
    new_container.push_front(element); // Add the new element to the *front* of the new list.

    // Reverse the new list
    new_container.reverse();

    // Concatenate the new element with the original list
    container.reverse();
    container.splice_after(container.before_begin(), new_container);
    container.reverse();
    
    return container;
}

// Stack - WARNING: O(n)
template <typename T>
std::stack<T> append(T element, std::stack<T> container) {
    std::stack<T> temp_stack;

    // Transfer elements from original stack to temp_stack in reverse order
    while (!container.empty()) {
        temp_stack.push(container.top());
        container.pop();
    }

    // Push the new element onto the now-empty original stack
    container.push(element);

    // Push the elements back from temp_stack to original stack
    while (!temp_stack.empty()) {
        container.push(temp_stack.top());
        temp_stack.pop();
    }

    return container;
}
