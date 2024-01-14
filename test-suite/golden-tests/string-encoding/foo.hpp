#ifndef __FOO_HPP__
#define __FOO_HPP__

std::string mlc_foo(std::string x){
  std::string y = x + "\n" + "c++: <\\,\",\f,\n,\r,\t>";
  return y;
}

#endif
