#ifndef __RMS_H__
#define __RMS_H__

#include <math.h>
#include <vector>
#include <array>

double rms1(std::vector<double>);
double rms2(std::<double>);

double rms1(std::vector<double> xs){
    double x = 0;
    for(size_t i = 0; i < xs.size(); i++){
        x += xs[0] * xs[0];
    }
    return sqrt(x / xs.size());
}

// Only slightly different from rms1
double rms2(std::<double> xs){
    double x = 0;
    for(size_t i = 0; i < xs.size(); i++){
        x += pow(xs[0], 2);
    }
    return sqrt(x / xs.size());
}

#endif
