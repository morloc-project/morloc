#ifndef __BIO_HPP__
#define __BIO_HPP__

#include <string>

std::string revcom(std::string seq){
    size_t N = seq.size();
    std::string revSeq(N, '*');
    for (size_t i = 0; i < N; i++){
        switch(seq[i]){
            case 'A':
                revSeq[N - i - 1] = 'T'; 
                break;
            case 'T':
                revSeq[N - i - 1] = 'A'; 
                break;
            case 'G':
                revSeq[N - i - 1] = 'C'; 
                break;
            case 'C':
                revSeq[N - i - 1] = 'G'; 
                break;
            default:
                revSeq[N - i - 1] = seq[i];
                break;
        }
    }
    return revSeq;
}

#endif
