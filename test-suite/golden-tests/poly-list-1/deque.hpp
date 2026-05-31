#include <deque>
#include <vector>

template <class A>
std::deque<A> morloc_packDeque(const std::vector<A>& xs) {
    return std::deque<A>(xs.begin(), xs.end());
}

template <class A>
std::vector<A> morloc_unpackDeque(const std::deque<A>& xs) {
    return std::vector<A>(xs.begin(), xs.end());
}
