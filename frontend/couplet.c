#include "couplet.h"

Couplet* couplet_new(struct W* lhs, struct W* rhs, char op){
    Couplet* c = (Couplet*)malloc(sizeof(Couplet));
    c->lhs = lhs;
    c->rhs = rhs;
    c->op = op;
    return c;
}

Couplet* couplet_copy(Couplet* c){
    return couplet_new(c->lhs, c->rhs, c->op);
}
