#pragma once
#include <string>
template <class A> struct BoxS { std::string foo; };
inline int inc(std::string s){ return (int)s.size(); }
inline BoxS<int> mk(int x){ return BoxS<int>{"abc"}; }
