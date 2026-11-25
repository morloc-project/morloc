#ifndef __MORLOC_PACKER_TEST_5_HPP__
#define __MORLOC_PACKER_TEST_5_HPP__

#include <vector>
#include <map>
#include <utility>
#include <cassert>

template <class A, class B>
std::map<A,B> morloc_packMap(std::tuple<std::vector<A>,std::vector<B>> items){
    std::map<A,B> m;
    std::vector<A> a = std::get<0>(items);
    std::vector<B> b = std::get<1>(items);
    assert(a.size() == b.size());
    for(std::size_t i = 0; i < a.size(); i++){
        m[a[i]] = b[i];
    }
    return m;
}

template <class A, class B>
std::tuple<std::vector<A>,std::vector<B>> morloc_unpackMap(std::map<A,B> m){
    std::vector<A> a;
    std::vector<B> b;
    for (auto tuple : m) {
        a.push_back(std::get<0>(tuple));
        b.push_back(std::get<1>(tuple));
    }
    return std::make_tuple(a, b);
}

template <class A, class B>
std::map<A,B> insert(std::map<A,B> m, A a, B b){
  m[a] = b;
  return m;
}

#endif
