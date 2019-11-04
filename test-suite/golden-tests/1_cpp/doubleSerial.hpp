#ifndef __SERIALIZERS_HPP__
#define __SERIALIZERS_HPP__

#include <stdio.h>

std::string packDouble(double x);
double unpackDouble(std::string json);

std::string packDouble(double x){
    return(std::to_string(x));
}

double unpackDouble(std::string json){
    return(std::stod(json.c_str()));
}

#endif
