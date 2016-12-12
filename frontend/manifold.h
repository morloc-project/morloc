#ifndef __MANIFOLD_H__
#define __MANIFOLD_H__

typedef struct ManifoldList{
    Manifold* manifold;
    Manifold* next;
} ManifoldList;

typedef struct Manifold{
    char* name;
    char* label;
    char* function;
    char** type;
    Manifold** inputs;
    Manifold** checks;
    Manifold** effects;
    char* cache ;
    char* unpack;
    char* run;
    char* fail;
    char* pack;
} Manifold;

Manifold* new_manifold(void);
void free_manifold(Manifold*);

#endif
