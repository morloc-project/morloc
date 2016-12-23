#include "manifold.h"

Manifold* manifold_new(char* name){
    static int uid = 0;
    Manifold* m = (Manifold*)calloc(1, sizeof(Manifold));
    m->name = name;
    m->uid = uid++;
    return m;
}
