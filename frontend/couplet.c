#include "couplet.h"

Couplet* couplet_new(struct W* lhs, struct W* rhs){
    Couplet* c = (Couplet*)malloc(sizeof(Couplet));
    c->lhs = lhs;
    c->rhs = rhs;
    return c;
}
