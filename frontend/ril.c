#include "ril.h"

RatStack* new_RatStack(){
     return (RatStack*)calloc(1,sizeof(RatStack));
}

void print_RIL(RatStack* rs){
    while( rs->doc->value ){
        Couplet* c = pop_Couplet(rs->doc);
        printf("DOC %s '%s'\n", c->name, c->value);
    }
}
