#pragma once
#include <chrono>
#include <thread>

int cnap(int seconds) {
    std::this_thread::sleep_for(std::chrono::seconds(seconds));
    return seconds;
}
