{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Translator.Source.CppInternals
Description : C++ serialization source code
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

The @serializationHandling@ code is copy-and-pasted from
@morloc-project/cppmorlocinternals/serial.hpp@. This is dreadful, I know, and I
will find an alternative solution soon.

-}



module Morloc.CodeGenerator.Grammars.Translator.Source.CppInternals
  ( foreignCallFunction
  , serializationHandling
  ) where

import Morloc.Quasi

foreignCallFunction = [idoc|
// Handle foreign calls. This function is used inside of C++ manifolds. Any
// changes in the name will require a mirrored change in the morloc code. 
std::string foreign_call(std::string cmd){
    char buffer[256];
    std::string result = "";
    FILE* pipe = popen(cmd.c_str(), "r");
    while (fgets(buffer, sizeof buffer, pipe) != NULL) {
        result += buffer;
    }
    pclose(pipe);
    return(result);
}
|]

serializationHandling = [idoc|
#include <iostream>
#include <sstream>
#include <string>
#include <stdexcept>
#include <stdio.h>
#include <vector>
#include <iomanip>
#include <limits>
#include <tuple>
#include <utility> 


std::string serialize(bool x, bool schema);
std::string serialize(int x, int schema);
std::string serialize(int x, size_t schema);
std::string serialize(int x, long schema);
std::string serialize(double x, double schema);
std::string serialize(float x, float schema);
std::string serialize(std::string x, std::string schema);

template <class A> std::string serialize(A x);

template <class... A>
std::string serialize(std::tuple<A...> x, std::tuple<A...> schema);

template <class A>
std::string serialize(std::vector<A> x, std::vector<A> schema);

bool match(const std::string json, const std::string pattern, size_t &i);
void whitespace(const std::string json, size_t &i);
std::string digit_str(const std::string json, size_t &i);
double read_double(std::string json);
float read_float(std::string json);

// attempt a run a parser, on failure, consume no input
template <class A>
bool try_parse(std::string json, size_t &i, A &x, bool (*f)(std::string, size_t &, A &));

bool deserialize(const std::string json, size_t &i, bool &x);
bool deserialize(const std::string json, size_t &i, double &x);
bool deserialize(const std::string json, size_t &i, float &x);
bool deserialize(const std::string json, size_t &i, std::string &x);

template <class A>
bool integer_deserialize(const std::string json, size_t &i, A &x);
bool deserialize(const std::string json, size_t &i, int &x);
bool deserialize(const std::string json, size_t &i, size_t &x);
bool deserialize(const std::string json, size_t &i, long &x);

template <class A>
bool deserialize(const std::string json, size_t &i, std::vector<A> &x);

template <class A>
bool _deserialize_tuple(const std::string json, size_t &i, std::tuple<A> &x);

template <class A, class... Rest>
bool _deserialize_tuple(const std::string json, size_t &i, std::tuple<A, Rest...> &x);

template <class... Rest>
bool deserialize(const std::string json, size_t &i, std::tuple<Rest...> &x);

template <class A>
A deserialize(const std::string json, A output);




/* ---------------------------------------------------------------------- */
/*                       S E R I A L I Z A T I O N                        */
/* ---------------------------------------------------------------------- */

std::string serialize(bool x, bool schema){
    return(x? "true" : "false");
}

std::string serialize(int x, int schema){
    std::ostringstream s;
    s << x;
    return(s.str());
}
std::string serialize(int x, size_t schema){
    std::ostringstream s;
    s << x;
    return(s.str());
}
std::string serialize(int x, long schema){
    std::ostringstream s;
    s << x;
    return(s.str());
}

std::string serialize(double x, double schema){
    std::ostringstream s;
    s << std::setprecision(std::numeric_limits<double>::digits10 + 2) << x;
    return(s.str());
}

std::string serialize(float x, float schema){
    std::ostringstream s;
    s << std::setprecision(std::numeric_limits<float>::digits10 + 2) << x;
    return(s.str());
}

std::string escape(const std::string& input) {
    std::string result;
    for (char c : input) {
        switch (c) {
            case '"':
                result += "\\\"";
                break;
            case '\\':
                result += "\\\\";
                break;
            case '\b':
                result += "\\b";
                break;
            case '\f':
                result += "\\f";
                break;
            case '\n':
                result += "\\n";
                break;
            case '\r':
                result += "\\r";
                break;
            case '\t':
                result += "\\t";
                break;
            // TODO fill in other special characters

            default:
                result += c;
        }
    }
    return result;
}


std::string serialize(std::string x, std::string schema){
    std::ostringstream s;
    s << '"' << escape(x) << '"';
    return(s.str());
}

template <class A>
std::string serialize(std::vector<A> x, std::vector<A> schema){
    A element_schema;
    std::ostringstream s;
    s << "[";
    for(size_t i = 0; i < x.size(); i++){
        s << serialize(x[i], element_schema);
        if((i+1) < x.size()){
            s << ',';
        }
    }
    s << "]";
    return (s.str());
}

template <class A>
std::string serialize(A x){
    return serialize(x, x);
}

// adapted from stackoverflow #1198260 answer from emsr
template<std::size_t I = 0, class... Rs>
inline typename std::enable_if<I == sizeof...(Rs), std::string>::type
  _serialize_tuple(std::tuple<Rs...> x)
  { return ""; }

template<std::size_t I = 0, class... Rs>
inline typename std::enable_if<I < sizeof...(Rs), std::string>::type
  _serialize_tuple(std::tuple<Rs...> x)
  {
    return serialize(std::get<I>(x)) + "," + _serialize_tuple<I + 1, Rs...>(x);
  }

template <class... A>
std::string serialize(std::tuple<A...> x, std::tuple<A...> schema){
    std::ostringstream ss;
    ss << "[";
    ss << _serialize_tuple(x);
    std::string json = ss.str();
    // _serialize_tuple adds a terminal comma, replaced here with the end bracket
    json[json.size() - 1] = ']';
    return json;
}


/* ---------------------------------------------------------------------- */
/*                             P A R S E R S                              */
/* ---------------------------------------------------------------------- */

// match a constant string, nothing is consumed on failure
bool match(const std::string json, const std::string pattern, size_t &i){
    for(size_t j = 0; j < pattern.size(); j++){
        if(j + i >= json.size()){
            return false;
        }
        if(json[j + i] != pattern[j]){
            return false;
        }
    }
    i += pattern.size();
    return true;
}

void whitespace(const std::string json, size_t &i){
    while(json[i] == ' ' || json[i] == '\n' || json[i] == '\t'){
        i++;
    }
}

// parse sequences of digits from a larger string
// used as part of a larger number parser
std::string digit_str(const std::string json, size_t &i){
    std::string num = "";
    while(json[i] >= '0' && json[i] <= '9'){
        num += json[i];
        i++;
    }
    return num;
}

double read_double(std::string json){
    return std::stod(json.c_str());
}

float read_float(std::string json){
    return std::stof(json.c_str());
}

// attempt a run a parser, on failure, consume no input
template <class A>
bool try_parse(std::string json, size_t &i, A &x, bool (*f)(std::string, size_t &, A &)){
    size_t j = i;
    if(f(json, i, x)){
        return true;
    } else {
        i = j;
        return false;
    }
}

/* ---------------------------------------------------------------------- */
/*                      D E S E R I A L I Z A T I O N                     */
/* ---------------------------------------------------------------------- */

// All combinator functions have the following general signature:
//
//   template <class A>
//   bool deserialize(const std::string json, size_t &i, A &x)

// The return value represents parse success.
// The index may be incremented even on failure.

// combinator parser for bool
bool deserialize(const std::string json, size_t &i, bool &x){
    if(match(json, "true", i)){
        x = true;
    }
    else if(match(json, "false", i)){
        x = false;
    }
    else {
        return false;
    }
    return true;
}

// combinator parser for doubles
bool deserialize(const std::string json, size_t &i, double &x){
    std::string lhs = "";
    std::string rhs = "";
    char sign = '+';
    
    if(json[i] == '-'){
        sign = '-';
        i++;
    }
    lhs = digit_str(json, i);
    if(json[i] == '.'){
        i++;
        rhs = digit_str(json, i);
    } else {
        rhs = "0";
    }

    if(lhs.size() > 0){
        x = read_double(sign + lhs + '.' + rhs);  
        return true;
    } else {
        return false;
    }
}

// combinator parser for floats
// FIXME: remove this code duplication
bool deserialize(const std::string json, size_t &i, float &x){
    std::string lhs = "";
    std::string rhs = "";
    char sign = '+';
    
    if(json[i] == '-'){
        sign = '-';
        i++;
    }
    lhs = digit_str(json, i);
    if(json[i] == '.'){
        i++;
        rhs = digit_str(json, i);
    } else {
        rhs = "0";
    }

    if(lhs.size() > 0){
        x = read_float(sign + lhs + '.' + rhs);  
        return true;
    } else {
        return false;
    }
}


// combinator parser for double-quoted strings
bool deserialize(const std::string json, size_t &i, std::string &x){
    try {
        x = "";
        if(! match(json, "\"", i)){
            throw 1;
        }
        bool escape = false;
        bool done = false;
        for(; i < json.size() && !done; i++){
            char c = json[i];
            if (escape) {
                switch (c) {
                    case '"':
                        x += '\"';
                        break;
                    case '\\':
                        x += '\\';
                        break;
                    case '/':
                        x += '/';
                        break;
                    case 'b':
                        x += '\b';
                        break;
                    case 'f':
                        x += '\f';
                        break;
                    case 'n':
                        x += '\n';
                        break;
                    case 'r':
                        x += '\r';
                        break;
                    case 't':
                        x += '\t';
                        break;
                    // TODO: add other escaped patterns

                    default:
                        x += '\\'; // Keep the backslash if it's not part of an escape sequence
                        x += c;
                }
                escape = false;
            } else {
                if (c == '\\') {
                    escape = true;
                } else if (c == '"'){
                    done = true;
                } else {
                    x += c;
                }
            }
        }
    } catch (int e) {
        return false;
    }
    return true;
}

template <class A>
bool integer_deserialize(const std::string json, size_t &i, A &x){
    char sign = '+';
    if(json[i] == '-'){
        sign = '-';
        i++;
    }
    std::string x_str = digit_str(json, i);
    if(x_str.size() > 0){
        std::stringstream sstream(sign + x_str);
        sstream >> x;
        return true;
    }
    return false; 
}
bool deserialize(const std::string json, size_t &i, int &x){
    return integer_deserialize(json, i, x);
}
bool deserialize(const std::string json, size_t &i, size_t &x){
    return integer_deserialize(json, i, x);
}
bool deserialize(const std::string json, size_t &i, long &x){
    return integer_deserialize(json, i, x);
}

// parser for vectors
template <class A>
bool deserialize(const std::string json, size_t &i, std::vector<A> &x){
    x = {};
    try {
        if(! match(json, "[", i)){
            throw 1;
        }
        whitespace(json, i);
        while(true){
            A element;
            if(deserialize(json, i, element)){
                x.push_back(element);
                whitespace(json, i);
                match(json, ",", i);
                whitespace(json, i);
            } else {
                break;
            }
        }
        whitespace(json, i);
        if(! match(json, "]", i)){
            throw 1;
        }
    } catch (int e) {
        return false;
    }
    return true;
}

template <class A>
bool _deserialize_tuple(const std::string json, size_t &i, std::tuple<A> &x){
    A a;
    if(! deserialize(json, i, a)){
        return false;
    }
    x = std::make_tuple(a);
    return true;
}
template <class A, class... Rest>
bool _deserialize_tuple(const std::string json, size_t &i, std::tuple<A, Rest...> &x){
    A a;
    // parse the next element
    if(! deserialize(json, i, a)){
        return false;
    }
    // skip whitespace and the comma
    whitespace(json, i);
    if(! match(json, ",", i)){
        return false;
    }
    whitespace(json, i);
    // parse the rest of the elements
    std::tuple<Rest...> rs;
    if(! _deserialize_tuple(json, i, rs)){
        return false;
    }
    // cons
    x = std::tuple_cat(std::make_tuple(a), rs);
    return true;
}
template <class... Rest>
bool deserialize(const std::string json, size_t &i, std::tuple<Rest...> &x){
    try {
        if(! match(json, "[", i)){
            throw 1;
        }
        whitespace(json, i);
        if(! _deserialize_tuple(json, i, x)){
            throw 1;
        }
        whitespace(json, i);
        if(! match(json, "]", i)){
            throw 1;
        }
    } catch (int e) {
        return false;
    }
    return true;
}

template <class A>
A deserialize(const std::string json, A output){
    size_t i = 0;
    deserialize(json, i, output);
    return output;
}

template <class... Rest>
std::tuple<Rest...> deserialize(const std::string json, std::tuple<Rest...> output){
    size_t i = 0;
    deserialize(json, i, output);
    return output;
}
|]
