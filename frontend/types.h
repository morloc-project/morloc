#ifndef __TYPES_H__
#define __TYPES_H__

#include <stdlib.h>
#include <stdio.h>

typedef struct Label{
    struct Label* next;
    char* name; 
} Label;
Label* new_Label(char* name);

typedef struct List{
    struct List* next;
    struct List* prev;
    void* value;
} List;

typedef struct NamedList{
    struct NamedList* next;
    struct NamedList* prev;
    Label* name;
    void* value;
} NamedList;

List* new_List();
NamedList* new_NamedList();

typedef struct Manifold{
    char* name;          /* the name used in the composition (NOT unique) */
    int uid;             /* a globally unique id */
    List* function; /* the name of the function (1 per language) */
    List* cache;    /* only one will be used usually, but it might make sense to have a value both in memory AND persistant storage */
    NamedList* args; /* a arguments */
    List* inputs;   /* b inputs */
    List* checks;   /* c checks */
    List* effects;  /* d effects */
    List* type;     /* b + 1 types */
    List* unpack;   /* 1 or b */
    List* run;      /* usually only one, maybe multiples if there are multiple placees it might run, then just use the first that works */
    List* fail;     /* list of things to do on failure */
    List* pack;     /* a bit hard to imagine why you would need multiples of these ... */
} Manifold;

Manifold* new_Manifold(char* name);

typedef enum { C_UNDEFINED, C_VARIABLE, C_POSITIONAL, C_GROUP, C_CONDITIONAL, C_NEST } ComposonType;

typedef struct Composon{
    ComposonType type;
    Manifold* manifold;
    union {
        char* name;
        List* nest;
    } value;
} Composon;

Composon* new_Composon(ComposonType type);


void rewind_path(NamedList* p);

#define REWIND(xs) do{ if(xs) {while(xs->prev != NULL) xs = xs->prev;} } while(0)
#define UPWIND(xs) do{ if(xs) {while(xs->next != NULL) xs = xs->next;} } while(0)

#define JOIN(xs, ys)                         \
do{                                          \
    UPWIND(xs);                              \
    REWIND(ys);                              \
    if(xs        != NULL &&                  \
       ys        != NULL &&                  \
       xs->value != NULL &&                  \
       ys->value != NULL                     \
    ){                                       \
        xs->next = ys;                       \
        ys->prev = xs;                       \
    } else {                                 \
        if(xs == NULL || xs->value == NULL){ \
            xs = ys;                         \
        }                                    \
    }                                        \
    UPWIND(xs);                              \
} while(0)

#define ADD(xs, ln_name, x, type)                             \
do {                                                          \
    UPWIND(xs);                                               \
    if(xs->value == NULL){                                    \
        xs->value = (type)(x);                                \
        xs->name = ln_name;                                   \
    } else {                                                  \
        NamedList* l = (NamedList*)malloc(sizeof(NamedList)); \
        l->value = (type)(x);                                 \
        l->name = ln_name;                                    \
        l->next = NULL;                                       \
        l->prev = xs;                                         \
        xs->next = l;                                         \
        xs = l;                                               \
    }                                                         \
} while(0)

#define LADD(xs, x, type)                      \
do {                                           \
    UPWIND(xs);                                \
    if(xs->value == NULL){                     \
        xs->value = (type)(x);                 \
    } else {                                   \
        List* l = (List*)malloc(sizeof(List)); \
        l->value = (type)(x);                  \
        l->next = NULL;                        \
        l->prev = xs;                          \
        xs->next = l;                          \
        xs = l;                                \
    }                                          \
} while(0)

#define COUPLET(xs, name, x) ADD(xs, name, x, Label*)

#endif
