#ifndef __MORLOC_DUMBY_HPP__
#define __MORLOC_DUMBY_HPP__

#include <string>
#include <functional>

// h :: (a -> b) -> Str -> Real
template <class L1, class L2>
double h(std::function<L2(L1)> f, std::string x){
  return 4.2;
}

#endif
