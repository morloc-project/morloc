#ifndef __FOO_HPP__
#define __FOO_HPP__

#include <string>

std::string cat(std::string x, std::string y){
  std::string z = x + y;
  return z;
}

int len(std::string x){
  return x.size();
}

#endif
