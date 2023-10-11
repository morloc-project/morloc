#include <algorithm>
#include <functional>

// f :: (a -> b) -> a -> b
// g :: Str -> Int
// h :: (Str, Int) -> Str

// f :: (a -> b) -> a -> b
template <typename a, typename b>
b f(std::function<b(a)> func, a x){
  return(func(x));
}

// g :: Bool -> Int
int g(bool x){
  return(42);
}
