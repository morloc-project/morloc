#pragma once
#include <string>
#include <vector>
#include <sys/wait.h>
#include <unistd.h>

// No serializer knows this type; only its packer gives it a wire form.
class Tally {
  public:
    explicit Tally(int x) : v(x) {}
    int get() const { return v; }
  private:
    int v;
};

inline Tally pack_tally(int x) { return Tally(x); }
inline int unpack_tally(const Tally& t) { return t.get(); }

template <typename F>
void produce(F sink) {
    sink(std::vector<Tally>{Tally(1), Tally(2)});
    sink(std::vector<Tally>{Tally(3)});
}

template <typename F>
void produce_forks(F sink) {
    sink(std::vector<Tally>{Tally(1)});
    pid_t pid = fork();
    if (pid == 0) {
        try {
            sink(std::vector<Tally>{Tally(10)});
        } catch (...) {
            _exit(3);
        }
        _exit(0);
    }
    int status = 0;
    waitpid(pid, &status, 0);
    sink(std::vector<Tally>{Tally(100)});
}

inline int add_tallies(int acc, const std::vector<Tally>& xs) {
    for (const auto& t : xs) acc += t.get();
    return acc;
}
inline int merge(int a, int b) { return a + b; }
inline std::string show_n(int n) { return "cpp n=" + std::to_string(n) + "\n"; }
