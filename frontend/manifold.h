#ifndef __MANIFOLD_H__
#define __MANIFOLD_H__

#include <stdlib.h>

typedef struct LL{
    struct LL* next;
    struct LL* prev;
    void* value; 
} LL;

typedef struct Manifold{
    LL* name;
    LL* function;
    LL* cache;
    LL* args;
    LL* inputs;
    LL* checks;
    LL* effects;
    LL* label;
    LL* type;
    LL* unpack;
    LL* run;
    LL* fail;
    LL* pack;
} Manifold;

typedef struct Cache{
    char* name;
    LL* args;
} Cache;

typedef struct Arg{
    char* lhs; 
    void* rhs;
    char* rhs_type;
} Arg;

LL*       new_LL();
Arg*      new_Arg();
Cache*    new_Cache();
Manifold* new_Manifold();

void free_LL(LL* ll, void (*free_value)(void*));

void vfree_char     ( void* vptr );
void vfree_Arg      ( void* vptr );
void vfree_Cache    ( void* vptr );
void vfree_Manifold ( void* vptr );

LL* add(LL* ll, void* value);

#endif
