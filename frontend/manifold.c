#include "manifold.h"

Manifold* manifold_new(){
    static int uid = -1;
    Manifold* m = (Manifold*)calloc(1,sizeof(Manifold));
    m->uid = ++uid;
    return m;
}
