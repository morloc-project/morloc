#ifndef __TYPES_H__
#define __TYPES_H__

#include <stdlib.h>
#include <stdio.h>

typedef struct List{
    struct List* next;
    struct List* prev;
    void* value;
} List;

typedef struct NamedList{
    struct NamedList* next;
    struct NamedList* prev;
    char* name;
    void* value;
} NamedList;

List* new_List();
NamedList* new_NamedList();

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

#define COUPLET(xs, name, x) ADD(xs, name, x, char*)

#endif
