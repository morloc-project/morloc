#ifndef __TYPES_H__
#define __TYPES_H__

#include <stdlib.h>
#include <stdio.h>

typedef struct List{
    struct List* next;
    struct List* prev;
    void* value;
} List;

typedef struct Couplet{
    char* name;
    char* value;
} Couplet;

typedef struct Source{
    char* lang;
    List* lines; 
} Source;

List* new_List();
Couplet* new_Couplet(char* name, char* value);
Source* new_Source(char* lang);


#define NEW_LIST (List*)calloc(1, sizeof(List));

#define ADD(xs, x, type)                   \
do {                                       \
    while(xs->next != NULL) xs = xs->next; \
    if(xs->value == NULL){                 \
        xs->value = (type)(x);             \
    } else {                               \
        List* new_list = NEW_LIST;         \
        new_list->value = (type)(x);       \
        new_list->next = NULL;             \
        new_list->prev = xs;               \
        xs->next = new_list;               \
        xs = new_list;                     \
    }                                      \
} while(0)

#define PUT_COUPLET(xs, n, v) ADD(xs, new_Couplet(n,v), Couplet*);

#define REWIND(xs) while(xs->prev != NULL) xs = xs->prev;

#endif
