#include <functional>
#include <vector>

struct DriveUnit {
    template <class T>
    operator T() const { return T{}; }
};

template <class Producer>
struct DriveCollected {
    Producer produce;
    template <class V>
    operator V() {
        V all;
        produce([&all](const V& batch) {
            all.insert(all.end(), batch.begin(), batch.end());
            return DriveUnit{};
        });
        return all;
    }
};

template <class Producer>
auto drive(Producer produce) {
    return DriveCollected<Producer>{std::move(produce)};
}

template <class A>
int count(const std::vector<A>& xs) {
    return static_cast<int>(xs.size());
}
