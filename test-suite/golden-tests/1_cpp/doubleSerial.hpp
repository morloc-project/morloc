#ifndef __SERIALIZERS_HPP__
#define __SERIALIZERS_HPP__

#include <stdio.h>

std::string packDouble(double x);
double unpackDouble(const char* json);

std::string packDouble(double x){
    return(std::to_string(x));
}

double unpackDouble(const char* json){
    return(std::stod(json));
}

#endif
