#pragma once
#include <cstdlib>
#include <fstream>
#include <stdexcept>
#include <string>
#include <vector>
#include <unistd.h>

// One integer per line, in batches of two. A line "bad" fails the parse and
// a line "die" kills the process; <path>.done is written once the whole input
// is read.
template <class Sink>
void produceC(const std::string& path, Sink sink) {
    std::ifstream in(path == "-" ? "/dev/stdin" : path);
    if (!in) throw std::runtime_error("cannot open " + path);
    std::vector<int> batch;
    std::string line;
    while (std::getline(in, line)) {
        if (line == "bad") throw std::runtime_error("bad line in " + path);
        if (line == "die") std::_Exit(3);
        batch.push_back(std::stoi(line));
        if (batch.size() == 2) { sink(batch); batch.clear(); }
    }
    if (!batch.empty()) sink(batch);
    if (path != "-") std::ofstream(path + ".done") << "done\n";
}

bool doneC(const std::string& path) { return access((path + ".done").c_str(), F_OK) == 0; }
int sumC(const std::vector<int>& xs) { int s = 0; for (int x : xs) s += x; return s; }
