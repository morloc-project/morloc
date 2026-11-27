#ifndef __CPPMORLOC_API_HPP__
#define __CPPMORLOC_API_HPP__

// The C++ binding's user-facing API (CPP-8). Every C++ pool includes this
// header before the sourced headers, so sourced code may call what it
// declares. Only the names listed under "User API" are a contract; the
// rest is used by generated code and may change.

#include <functional>
#include <memory>
#include <type_traits>
#include <utility>

namespace mlc {

// Generated code: a closure with a by-value form held as a std::function
// (OWN-7, CPP-7).

template <class P> struct given_param { using type = P; };
template <class A> struct given_param<const A&> { using type = A&&; };
template <class P> using given_param_t = typename given_param<P>::type;

template <class Sig> class owning;

template <class R, class... P>
class owning<R(P...)> {
    struct base {
        virtual ~base() = default;
        virtual R lend(P... a) const = 0;
        virtual R take(given_param_t<P>... a) const = 0;
    };
    template <class F>
    struct impl final : base {
        F f;
        explicit impl(F g) : f(std::move(g)) {}
        R lend(P... a) const override { return f(a...); }
        R take(given_param_t<P>... a) const override { return f(std::forward<given_param_t<P>>(a)...); }
    };
    std::shared_ptr<const base> p_;

public:
    template <class F>
    explicit owning(F f) : p_(std::make_shared<const impl<F>>(std::move(f))) {}
    R operator()(P... a) const { return p_->lend(a...); }
    R operator()(given_param_t<P>... a) const
        requires(!(std::is_same_v<P, given_param_t<P>> && ...))
    {
        return p_->take(std::forward<given_param_t<P>>(a)...);
    }
};

template <class F> struct function_sig {};
template <class R, class... P> struct function_sig<std::function<R(P...)>> { using type = R(P...); };

// User API

// MLC_GIVE: defined when mlc::give is available.
#define MLC_GIVE 1

// mlc::give(f, std::move(x)...): call the function value f, handing it
// ownership of its arguments (CPP-1, CPP-8).
template <class F, class... A>
decltype(auto) give(F&& f, A&&... a) {
    static_assert((!std::is_lvalue_reference_v<A> && ...), "mlc::give takes rvalues only");
    using Fd = std::remove_cvref_t<F>;
    if constexpr (requires { typename function_sig<Fd>::type; }) {
        using Own = owning<typename function_sig<Fd>::type>;
        if (const Own* o = f.template target<Own>()) return (*o)(std::move(a)...);
    }
    return std::forward<F>(f)(std::move(a)...);
}

}  // namespace mlc

#endif
