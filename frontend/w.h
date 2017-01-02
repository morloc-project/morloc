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
    P_COUPLET,
    P_MANIFOLD,
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
    V_LABEL,
    V_MANIFOLD
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

W* w_print(const W* w);

VType get_value_type(Class cls);

char* w_class_str(Class);

char* w_type_str(VType);

bool w_is_recursive(const W* w);

void w_assert_class(const W*, Class);

void w_assert_type(const W*, VType);

       char*     g_string   (const W* w);
struct Ws*       g_ws       (const W* w);
struct Couplet*  g_couplet  (const W* w);
struct Label*    g_label    (const W* w);
struct Manifold* g_manifold (const W* w);
       W*        g_lhs      (const W* w);
       W*        g_rhs      (const W* w);

void s_ws       (W* w, struct Ws* v);
void s_couplet  (W* w, Couplet*   v);
void s_label    (W* w, Label*     v);
void s_manifold (W* w, Manifold*  v);
void s_lhs      (W* w, W*         v);
void s_rhs      (W* w, W*         v);

#endif
