#ifndef __TAIL_LOOP_HOST_CALLBACK_HPP__
#define __TAIL_LOOP_HOST_CALLBACK_HPP__

#include <functional>
#include <vector>

// Hand a native (non-morloc) callback to a morloc producer.
template <class Producer>
int hostDrive(Producer produce) {
    std::function<int(std::vector<int>)> sink = [](std::vector<int> xs) {
        int total = 0;
        for (int x : xs) total += x;
        return total;
    };
    return produce(sink);
}

#endif
