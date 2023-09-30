#include <algorithm>
#include <functional>

// f :: (a -> b) -> a -> b
template <typename a, typename b>
b f(std::function<b(a)> func, a){
  return(func(a));
}

// g :: Str -> Int
int g(std::string x){
  return(x.size());
}
