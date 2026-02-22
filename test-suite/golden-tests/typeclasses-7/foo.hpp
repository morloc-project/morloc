#ifndef __FOO_HPP__
#define __FOO_HPP__

#include <string>

int appendInt(int x, int y){
  return x + y;
}

std::string appendStr(std::string x, std::string y){
  return x + y;
}

std::string showInt(int x){
  return std::to_string(x);
}

std::string showStr(std::string x){
  return x;
}

#endif
