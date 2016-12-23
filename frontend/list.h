#ifndef __LIST_H__
#define __LIST_H__

#include "label.h"

#include <stdlib.h>
#include <stdio.h>

typedef struct List{
    struct List* next;
    struct List* prev;
    void* value;
} List;

List* list_new();

typedef struct NamedList{
    struct NamedList* next;
    struct NamedList* prev;
    Label* name;
    void* value;
} NamedList;

NamedList* namedlist_new();

void rewind_path(NamedList* p);

void print_couplet(NamedList* l, char* cmd);

void print_list(List* l, char* cmd);

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

#define COUPLET(xs, name, x) ADD(xs, name, x, Label*)

#endif
