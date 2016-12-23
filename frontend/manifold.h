#ifndef __MANIFOLD_H__
#define __MANIFOLD_H__

#include "list.h"

#include <stdlib.h>

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

Manifold* manifold_new(char* name);

#endif
