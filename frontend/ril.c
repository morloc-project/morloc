#include "ril.h"

RatStack* new_RatStack(){
     RatStack* rs = (RatStack*)calloc(1,sizeof(RatStack));
     rs->doc   = new_CoupletStack();
     rs->alias = new_CoupletStack();
     rs->cache = new_CoupletStack();
     rs->pack  = new_CoupletStack();
     rs->open  = new_CoupletStack();
     rs->fail  = new_CoupletStack();
     rs->pass  = new_CoupletStack();
     return rs;
}

void print_RIL(RatStack* rs){
    while( rs->doc->value ){
        Couplet* c = pop_Couplet(rs->doc);
        printf("DOC %s '%s'\n", c->name, c->value);
    }
    while( rs->alias->value ){
        Couplet* c = pop_Couplet(rs->alias);
        printf("ALIAS %s %s\n", c->name, c->value);
    }
    while( rs->cache->value ){
        Couplet* c = pop_Couplet(rs->cache);
        printf("CACHE %s %s\n", c->name, c->value);
    }
    while( rs->pack->value ){
        Couplet* c = pop_Couplet(rs->pack);
        printf("PACK %s %s\n", c->name, c->value);
    }
    while( rs->open->value ){
        Couplet* c = pop_Couplet(rs->open);
        printf("OPEN %s %s\n", c->name, c->value);
    }
    while( rs->fail->value ){
        Couplet* c = pop_Couplet(rs->fail);
        printf("FAIL %s %s\n", c->name, c->value);
    }
    while( rs->pass->value ){
        Couplet* c = pop_Couplet(rs->pass);
        printf("PASS %s %s\n", c->name, c->value);
    }
}
