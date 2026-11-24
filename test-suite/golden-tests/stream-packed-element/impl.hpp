#pragma once
#include <string>
#include <vector>

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
    sink(std::vector<Tally>{});
    sink(std::vector<Tally>{Tally(3)});
}

inline std::string show_all(const std::vector<Tally>& xs) {
    std::string out = "cpp";
    for (const auto& t : xs) out += " " + std::to_string(t.get());
    return out + "\n";
}

inline std::string show_batch(const std::vector<Tally>& xs) {
    return "cpp batch " + std::to_string(xs.size()) + "\n";
}
