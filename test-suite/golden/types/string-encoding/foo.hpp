#ifndef __FOO_HPP__
#define __FOO_HPP__

template <class A>
std::string mlc_foo(A x){
  A y = x + "\n" + "c++: <\\,\",\f,\n,\r,\t,草泥马>";
  return y;
}

#endif
