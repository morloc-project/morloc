#ifndef __W_H__
#define __W_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <err.h>

#include "label.h"
#include "manifold.h"
#include "couplet.h"

typedef enum {
    X_NONE=0,     // X for special, I suppose
    P_STRING,     // P for primitive
    P_WS,
    P_COUPLET,
    P_MANIFOLD,
    T_EFFECT,     // T for top level
    T_HOOK,
    T_CHECK,
    T_OPEN,
    T_PACK,
    T_PASS,
    T_FAIL,
    T_ALIAS,
    T_LANG,
    T_DOC,
    T_CACHE,
    T_PATH,       
    T_EXPORT,
    T_SOURCE,
    T_ARGUMENT,
    P_ARGUMENT,
    T_TYPE,
    T_ONTOLOGY,
    C_COMPOSON,   // C for composition
    C_MANIFOLD,
    C_NEST,
    C_GRPREF,
    C_POSITIONAL,
    C_ARGREF,
    C_DEREF,
    C_REFER,
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

W* w_isolate(W* w);

W* w_copy(W* w);

// clone a W while preserving class and value
// this is recursive, next is cloned as well
W* w_clone(W* w);

W* w_print(W* w);

VType get_value_type(Class cls);

char* w_class_str(Class);

char* w_type_str(VType);

void w_assert_class(W*, Class);

void w_assert_type(W*, VType);

// Get the vaue of w
       char*     g_string   (W* w);
struct Ws*       g_ws       (W* w);
struct Couplet*  g_couplet  (W* w);
struct Label*    g_label    (W* w);
struct Manifold* g_manifold (W* w);
       W*        g_lhs      (W* w);
       W*        g_rhs      (W* w);

// Set the value of w, fails if v is not compatible with type(w)
void s_none     (W* w              );
void s_string   (W* w, char* v     );
void s_ws       (W* w, struct Ws* v);
void s_couplet  (W* w, Couplet*   v);
void s_label    (W* w, Label*     v);
void s_manifold (W* w, Manifold*  v);
void s_lhs      (W* w, W*         v);
void s_rhs      (W* w, W*         v);

// Set the value of W and change the class
// The current class of w is ignored
// These do check to ensure Class c is fits type of v
void force_set_none     ( W* w                        );
void force_set_string   ( W* w, Class c, char*      v );
void force_set_ws       ( W* w, Class c, struct Ws* v );
void force_set_couplet  ( W* w, Class c, Couplet*   v );
void force_set_label    ( W* w, Class c, Label*     v );
void force_set_manifold ( W* w, Class c, Manifold*  v );

#endif
