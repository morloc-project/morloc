#include <functional>
#include <stdexcept>
#include <thread>

// Calls f on a thread this code starts; a failure there belongs to no call.
int applyInThread(std::function<int(int)> f, int x) {
    int r = 0;
    std::thread t([&] { r = f(x); });
    t.join();
    return r;
}

int throwInThread(int x) {
    std::thread t([] { throw std::runtime_error("boom on a user thread"); });
    t.join();
    return x;
}
