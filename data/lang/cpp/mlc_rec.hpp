#ifndef MLC_REC_HPP
#define MLC_REC_HPP

#include <memory>
#include <new>
#include <type_traits>
#include <utility>

// Constructor arms made by `mlc::make_arm` are released in bounded stack
// (STATE-8).

namespace mlc {

constexpr int arm_max_depth = 128;

#if defined(__ELF__)
extern thread_local constinit int arm_depth_ __attribute__((tls_model("local-exec")));
#else
extern thread_local constinit int arm_depth_;
#endif

// Queue `p` to be freed by `free_fn` when the outermost release drains;
// false when the worklist cannot grow.
bool arm_defer(void (*free_fn)(void*), void* p) noexcept;
// Free every queued object, and every object their release queues.
void arm_drain() noexcept;

template <class U> void arm_delete(void* p) noexcept { delete static_cast<U*>(p); }

template <class T> struct arm_allocator {
    using value_type = T;
    arm_allocator() noexcept = default;
    template <class U> arm_allocator(const arm_allocator<U>&) noexcept {}
    T* allocate(std::size_t n) { return std::allocator<T>{}.allocate(n); }
    void deallocate(T* p, std::size_t n) noexcept { std::allocator<T>{}.deallocate(p, n); }
    template <class U, class... A> void construct(U* p, A&&... a) {
        if constexpr (std::is_constructible_v<U, A...>) {
            ::new (static_cast<void*>(p)) U(std::forward<A>(a)...);
        } else {
            ::new (static_cast<void*>(p)) U{std::forward<A>(a)...};
        }
    }
    template <class U> void destroy(U* p) noexcept {
        if (arm_depth_ >= arm_max_depth && defer_contents(p)) return;
        ++arm_depth_;
        p->~U();
        if (arm_depth_ == 1) arm_drain();
        --arm_depth_;
    }
    // STATE-8: past the bound, the contents go to the worklist in a heap
    // object of their own, moved or else copied; false if neither is made.
    template <class U> static bool defer_contents(U* p) noexcept {
        U* kept = nullptr;
        if constexpr (std::is_move_constructible_v<U>) {
            try { kept = new U(std::move(*p)); } catch (...) { kept = nullptr; }
        } else if constexpr (std::is_copy_constructible_v<U>) {
            try { kept = new U(*p); } catch (...) { kept = nullptr; }
        }
        if (kept == nullptr) return false;
        p->~U();
        if (!arm_defer(&arm_delete<U>, kept)) {
            ++arm_depth_;
            delete kept;
            --arm_depth_;
        }
        return true;
    }
    template <class U> bool operator==(const arm_allocator<U>&) const noexcept { return true; }
    template <class U> bool operator!=(const arm_allocator<U>&) const noexcept { return false; }
};

// A constructor arm (or any recursive node), released per STATE-8.
template <class T, class... A> std::shared_ptr<T> make_arm(A&&... a) {
    return std::allocate_shared<T>(arm_allocator<T>{}, std::forward<A>(a)...);
}

// The arm behind a held pointer, const when the holder is (STATE-8).
template <class T> T* arm(std::shared_ptr<T>& p) noexcept { return p.get(); }
template <class T> const T* arm(const std::shared_ptr<T>& p) noexcept { return p.get(); }

} // namespace mlc

#endif // MLC_REC_HPP
