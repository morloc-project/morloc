#pragma once
#include <tuple>
#include <utility>

template <class A, class B>
B snd2(const std::pair<A, B>& p) { return p.second; }

template <class A, class B>
A fst2(const std::pair<A, B>& p) { return p.first; }

template <class A, class B>
B sndT(const std::tuple<A, B>& p) { return std::get<1>(p); }
