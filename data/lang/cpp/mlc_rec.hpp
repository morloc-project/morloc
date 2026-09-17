#ifndef MLC_REC_HPP
#define MLC_REC_HPP

#include <memory>
#include <new>
#include <vector>

// ============================================================
// Deferred release of recursive values
// ============================================================
//
// A generated `data` type keeps each constructor's fields behind a
// `mlc::rec_ptr`, and those fields may hold the type again, so releasing
// the last owner of a deep chain would otherwise run one destructor frame
// per level and overflow the stack at a depth the heap could easily hold.
// While one release is draining, every further last-owner release on the
// thread hands its block to the drain's worklist and returns; the outermost
// release pops and frees until the list is empty. Depth then costs heap.
//
// A user-mapped recursive record may hold its `?Self` field in a
// `mlc::rec_ptr<Self>` to get the same guarantee; the marshallers treat it
// exactly as a `std::shared_ptr` slot.

namespace mlc {

struct rec_release {
    // Trivially destructible thread-locals: a release can run while other
    // thread-local and static objects are being destroyed at exit.
    static bool& active() noexcept { thread_local bool a = false; return a; }
    static std::vector<std::shared_ptr<void>>*& queue() noexcept {
        thread_local std::vector<std::shared_ptr<void>>* q = nullptr;
        return q;
    }
    // Hand a block to the running drain. May throw std::bad_alloc when the
    // worklist cannot grow; the caller then releases synchronously.
    static void enqueue(std::shared_ptr<void> p) {
        auto*& q = queue();
        if (!q) q = new std::vector<std::shared_ptr<void>>();
        q->push_back(std::move(p));
    }
    struct drain_guard {
        drain_guard() noexcept { active() = true; }
        ~drain_guard() {
            auto* q = queue();
            if (q) {
                while (!q->empty()) {
                    std::shared_ptr<void> p = std::move(q->back());
                    q->pop_back();
                    p.reset();  // its fields' rec_ptrs enqueue rather than recurse
                }
            }
            active() = false;
        }
    };
};

template<typename T>
class rec_ptr {
    std::shared_ptr<T> p_;
public:
    using element_type = T;
    rec_ptr() noexcept = default;
    // Neither a template nor explicit: a std::variant whose alternatives
    // are rec_ptrs must convert a shared_ptr<T> to exactly one of them.
    rec_ptr(std::shared_ptr<T> p) noexcept : p_(std::move(p)) {}
    rec_ptr(const rec_ptr& o) noexcept : p_(o.p_) {}
    rec_ptr(rec_ptr&& o) noexcept : p_(std::move(o.p_)) {}
    // Assignment releases the old pointee through the drain, not through
    // shared_ptr's own assignment, which would free it synchronously.
    rec_ptr& operator=(const rec_ptr& o) noexcept {
        if (this != &o) { release(); p_ = o.p_; }
        return *this;
    }
    rec_ptr& operator=(rec_ptr&& o) noexcept {
        if (this != &o) { release(); p_ = std::move(o.p_); }
        return *this;
    }
    ~rec_ptr() { release(); }
    T& operator*() const noexcept { return *p_; }
    T* operator->() const noexcept { return p_.get(); }
    T* get() const noexcept { return p_.get(); }
    explicit operator bool() const noexcept { return static_cast<bool>(p_); }
    void reset() noexcept { release(); }
    long use_count() const noexcept { return p_.use_count(); }
private:
    void release() noexcept {
        if (!p_) return;
        // A shared block only loses this owner's count; the last owner
        // frees it, and only that release can recurse.
        if (p_.use_count() != 1) { p_.reset(); return; }
        if (rec_release::active()) {
            try { rec_release::enqueue(std::move(p_)); return; }
            catch (const std::bad_alloc&) { p_.reset(); return; }
        }
        rec_release::drain_guard g;
        p_.reset();
    }
};

template<typename T> struct is_rec_ptr : std::false_type {};
template<typename T> struct is_rec_ptr<rec_ptr<T>> : std::true_type {};

} // namespace mlc

#endif // MLC_REC_HPP
