#ifndef __FOO_HPP__
#define __FOO_HPP__

#include <vector>
#include <deque>
#include <tuple>

// --- List (std::vector) operations ---

template <typename T>
std::vector<T> appendList(std::vector<T> a, std::vector<T> b) {
    a.insert(a.end(), b.begin(), b.end());
    return a;
}

template <typename T>
T listAt(int i, std::vector<T> xs) {
    return xs[i];
}

template <typename T>
std::vector<T> listCons(T x, std::vector<T> xs) {
    xs.insert(xs.begin(), x);
    return xs;
}

template <typename T>
std::tuple<T, std::vector<T>> listUncons(std::vector<T> xs) {
    T head = xs.front();
    std::vector<T> tail(xs.begin() + 1, xs.end());
    return {head, tail};
}

template <typename T>
std::vector<T> listSnoc(std::vector<T> xs, T x) {
    xs.push_back(x);
    return xs;
}

template <typename T>
std::tuple<std::vector<T>, T> listUnsnoc(std::vector<T> xs) {
    T last = xs.back();
    xs.pop_back();
    return {xs, last};
}

// --- Deque (std::deque) operations ---

template <typename T>
std::deque<T> appendDeque(std::deque<T> a, std::deque<T> b) {
    a.insert(a.end(), b.begin(), b.end());
    return a;
}

template <typename T>
std::deque<T> dequeCons(T x, std::deque<T> xs) {
    xs.push_front(x);
    return xs;
}

template <typename T>
std::tuple<T, std::deque<T>> dequeUncons(std::deque<T> xs) {
    T head = xs.front();
    xs.pop_front();
    return {head, xs};
}

template <typename T>
std::deque<T> dequeSnoc(std::deque<T> xs, T x) {
    xs.push_back(x);
    return xs;
}

template <typename T>
std::tuple<std::deque<T>, T> dequeUnsnoc(std::deque<T> xs) {
    T last = xs.back();
    xs.pop_back();
    return {xs, last};
}

#endif
