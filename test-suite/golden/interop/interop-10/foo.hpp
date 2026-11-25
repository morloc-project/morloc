#include <algorithm>
#include <functional>
#include <type_traits>

// f :: (a -> b) -> a -> b
// g :: Str -> Int
// h :: (Str, Int) -> Str

// f :: (a -> b) -> a -> b
template <typename a, typename F>
auto f(F func, a x) -> std::invoke_result_t<F, a> {
  return(func(x));
}

// g :: Bool -> Int
int g(bool x){
  return(42);
}
