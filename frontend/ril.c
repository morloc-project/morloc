#include "ril.h"

RatStack* new_RatStack(){
     RatStack* rs = (RatStack*)calloc(1,sizeof(RatStack));
     rs->export = new_List();
     rs->doc    = new_List();
     rs->alias  = new_List();
     rs->cache  = new_List();
     rs->pack   = new_List();
     rs->open   = new_List();
     rs->fail   = new_List();
     rs->pass   = new_List();
     /* rs->source = NULL; */
     return rs;
}

void rewind_RatStack(RatStack* rs){
    while( rs->export->prev ) rs->export = rs->export->prev ;
    while( rs->doc->prev    ) rs->doc    = rs->doc->prev    ;
    while( rs->alias->prev  ) rs->alias  = rs->alias->prev  ;
    while( rs->cache->prev  ) rs->cache  = rs->cache->prev  ;
    while( rs->pack->prev   ) rs->pack   = rs->pack->prev   ;
    while( rs->open->prev   ) rs->open   = rs->open->prev   ;
    while( rs->fail->prev   ) rs->fail   = rs->fail->prev   ;
    while( rs->pass->prev   ) rs->pass   = rs->pass->prev   ;
}

void print_couplet(List* l, char* cmd){
    for( ; l; l = l->next ){
        Couplet* c = (Couplet*) l->value;
        printf("%s %s %s\n", cmd, c->name, c->value);
    }
}

void print_RIL(RatStack* rs){

    rewind_RatStack(rs);

    print_couplet(rs->export, "EXPORT");
    print_couplet(rs->doc,    "DOC");
    print_couplet(rs->alias,  "ALIAS");
    print_couplet(rs->cache,  "CACHE");
    print_couplet(rs->pack,   "PACK");
    print_couplet(rs->open,   "OPEN");
    print_couplet(rs->fail,   "FAIL");
    print_couplet(rs->pass,   "PASS");
}
