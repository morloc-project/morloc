#pragma once
// A payload that checks how generated code handles it: every value has an
// identity in a live set; a move hands the identity over and marks the
// source; a read of a moved or destroyed value, a copy of one, and a second
// destruction each abort. Copies are counted. Unpacking a payload for
// output adds 1000 per copy made so far, so copies made on the way out show.
#include <atomic>
#include <cstdio>
#include <cstdlib>
#include <mutex>
#include <tuple>
#include <unordered_set>

namespace pay_detail {
inline std::mutex& lock() { static std::mutex m; return m; }
inline std::unordered_set<long>& live() { static auto* s = new std::unordered_set<long>(); return *s; }
inline std::atomic<long> next_id{1};
inline std::atomic<long> copies{0};
[[noreturn]] inline void die(const char* what, long id) {
    std::fprintf(stderr, "PAY VIOLATION: %s (id %ld)\n", what, id);
    std::fflush(stderr);
    std::_Exit(99);
}
inline long fresh() {
    long id = next_id.fetch_add(1);
    std::lock_guard<std::mutex> g(lock());
    live().insert(id);
    return id;
}
inline void check(long id, const char* what) {
    if (id == -1) die(what, id);
    std::lock_guard<std::mutex> g(lock());
    if (!live().count(id)) die(what, id);
}
}

class Pay {
  public:
    Pay() : key_(0), id_(pay_detail::fresh()) {}
    explicit Pay(int k) : key_(k), id_(pay_detail::fresh()) {}
    Pay(const Pay& o) : key_(o.key_) {
        pay_detail::check(o.id_, "copy of a moved or destroyed payload");
        id_ = pay_detail::fresh();
        pay_detail::copies.fetch_add(1);
    }
    Pay(Pay&& o) noexcept : key_(o.key_), id_(o.id_) {
        if (o.id_ != -1) pay_detail::check(o.id_, "move of a destroyed payload");
        o.id_ = -1;
    }
    Pay& operator=(const Pay& o) {
        if (this == &o) return *this;
        pay_detail::check(o.id_, "copy of a moved or destroyed payload");
        release();
        key_ = o.key_;
        id_ = pay_detail::fresh();
        pay_detail::copies.fetch_add(1);
        return *this;
    }
    Pay& operator=(Pay&& o) noexcept {
        if (this == &o) return *this;
        if (o.id_ != -1) pay_detail::check(o.id_, "move of a destroyed payload");
        release();
        key_ = o.key_;
        id_ = o.id_;
        o.id_ = -1;
        return *this;
    }
    ~Pay() { release(); id_ = -2; }
    int key() const {
        pay_detail::check(id_, "read of a moved or destroyed payload");
        return key_;
    }
  private:
    void release() {
        if (id_ == -1) return;
        if (id_ == -2) pay_detail::die("destroyed twice", id_);
        std::lock_guard<std::mutex> g(pay_detail::lock());
        if (!pay_detail::live().erase(id_)) pay_detail::die("destroyed twice", id_);
    }
    int key_;
    long id_;
};

inline Pay pay_pack(int k) { return Pay(k); }
inline int pay_unpack(const Pay& p) { return p.key() + 1000 * (int)pay_detail::copies.load(); }
inline Pay mkp(int k) { return Pay(k); }
inline int keyp(const Pay& p) { return p.key(); }
inline int keyv(Pay p) { return p.key(); }
inline std::tuple<int, int> census(int v) { return std::make_tuple(v, (int)pay_detail::copies.load()); }
inline std::tuple<Pay, int> mktp(int k) { return std::make_tuple(Pay(k), 1); }
