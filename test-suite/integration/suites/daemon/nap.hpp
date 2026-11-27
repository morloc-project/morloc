#pragma once
#include <chrono>
#include <thread>

double nap(double seconds) {
    std::this_thread::sleep_for(std::chrono::duration<double>(seconds));
    return seconds;
}
