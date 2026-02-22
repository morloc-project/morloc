#ifndef __SYSINFO_HPP__
#define __SYSINFO_HPP__

#include <string>
#include <iostream>

int clockResNs() {
    std::cout << "CLOCK_QUERY" << std::endl;
    return 1;
}

int cpuCount() {
    std::cout << "CPU_QUERY" << std::endl;
    return 4;
}

int hashStr(std::string s) {
    int hash = 0;
    for (char c : s) {
        hash = (hash * 31 + c) % 1000003;
    }
    return hash;
}

#endif
