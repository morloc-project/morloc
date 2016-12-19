#include "types.h"

void put_Couplet(CoupletStack* cs, char* name, char* value){
    Couplet* c = (Couplet*)malloc(sizeof(Couplet));
    c->name = name;
    c->value = value;

    if(cs->value){
       CoupletStack* n = (CoupletStack*)malloc(sizeof(CoupletStack));
       n->next = cs->next; 
       cs->next = n;
       n->value = cs->value;
    }
    cs->value = c;
}

Couplet* pop_Couplet(CoupletStack* cs){
    Couplet* top = NULL;
    if(cs){
        top = cs->value;
        if(cs->next){
            cs->value = cs->next->value;
            cs->next = cs->next->next;
            free(cs->next);
        } else {
            cs->value = NULL;
        }
    }
    return top;
}

CoupletStack* new_CoupletStack(){
    return (CoupletStack*)calloc(1,sizeof(CoupletStack));
}
