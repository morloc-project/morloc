#ifndef __W_H__
#define __W_H__

#include "stdio.h"
#include "stdlib.h"
#include <string.h>

#include "types.h"

typedef enum {
    X_NONE=0,     // X for special, I suppose
    P_STRING,     // P for primitive
    P_WS,
    T_EFFECT,     // T for top level
    T_PATH,       
    C_COMPOSON,   // C for composition
    C_MANIFOLD,
    C_NEST,
    C_GRPREF,
    C_POSITIONAL,
    C_DEREF,
    K_LIST,       // K for key
    K_PATH,
    K_LABEL,
    K_NAME
} Class;

typedef enum {
    V_NONE = 0,
    V_STRING,
    V_WS,
    V_COUPLET,
    V_LABEL
} VType;

typedef struct W{
    Class cls;
    int uid;
    struct W* next;
    union {
        void* none;
        char* string;
        struct Ws* ws;
        struct Couplet* couplet;
        struct Label* label;
        struct Manifold* manifold;
    } value;
} W;

W* w_new(Class cls, void* value);

W* w_isolate(const W* w);

W* w_copy(const W* w);

char* w_str(const W* w);

VType get_value_type(Class cls);

#endif
