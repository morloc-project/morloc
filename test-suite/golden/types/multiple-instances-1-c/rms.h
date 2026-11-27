#ifndef __RMS_H__
#define __RMS_H__

#include <math.h>
#include <vector>
#include <array>

double rms1(std::vector<double>);
double rms2(std::vector<double>);

double rms1(std::vector<double> xs){
    double x = 0;
    for(size_t i = 0; i < xs.size(); i++){
        x += xs[i] * xs[i];
    }
    return sqrt(x / xs.size());
}

// Only slightly different from rms1
double rms2(std::vector<double> xs){
    double x = 0;
    for(size_t i = 0; i < xs.size(); i++){
        x += pow(xs[i], 2);
    }
    return sqrt(x / xs.size());
}

#endif
