#ifndef __FOO_HPP__
#define __FOO_HPP__

template<typename A, typename C, typename F>
C g(F down, C b, A x){
    C b2 = down(x, b);
    return b2;
}

#endif
