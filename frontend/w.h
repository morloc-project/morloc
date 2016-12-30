#ifndef __CLASS_H__
#define __CLASS_H__


typedef enum {
    X_NONE=0,     // X for special, I suppose
    P_STRING,     // P for primitive
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

typedef struct W{
    Class cls;
    int uid;
    W* next;
    union {
        void* none;
        char* string;
        W* whatever;
        struct Couplet* entry;
        struct Label* label;
        struct Manifold* manifold;
        struct Effect* effect;
    } value;
} W;

// W* w_new(Class cls, void* value);
//
// void w_free(W* o);
//
// W* w_copy(W* o);
//
// W* w_clone(W* o);
//
// W* w_isolate(W* o);
//
// W* w_set_value(W* o, void* v);
//
// W* w_add_over(W* a, W* b);
//
// W* w_add_down(W* a, W* b);
//
// W* w_next(const W* o);
//
// W* w_down(const W* o);
//
// W* w_head(const W* o);
//
// W* w_tail(const W* o);
//
// W* w_map( const W* o, W*(*map)(W*) );
//
// W* w_dmap( const W* o, W*(*map)(W*), int depth );
//
// W* w_rmap( const W* o, W*(*map)(W*) );
//
/* Recursive map function with recursion rule (digger). The recursion rule
 * function takes a W* and derives the lower W* from the original value. */
// W* w_dig( const W* o, W*(*digger)(W*), W*(*map)(W*) );
//
// [> performs a pairwise function on two W's <]
// W* w_zmap( const W* a, const W* b, W*(*map)(W*, W*) );
//
// W* w_zdmap( const W* a, const W* b, W*(*map)(W*, W*), int depth );
//
// W* w_zrmap( const W* a, const W* b, W*(*map)(W*, W*) );
//
// W* w_filter( const W* o, bool(*filter)(W*) );
//
// W* w_flatten(const W* o);
//
// bool w_is_alone(const W* o);
//
// bool w_is_iterative(const W* o);
//
// bool w_is_recursive(const W* o);
//
// bool w_is_collective(const W* o);
//
// bool w_is_homogenous(const W* o);

#endif
