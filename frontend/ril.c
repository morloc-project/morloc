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
     rs->source = new_List();
     return rs;
}

void rewind_RatStack(RatStack* rs){
    REWIND( rs->export );
    REWIND( rs->doc    );
    REWIND( rs->alias  );
    REWIND( rs->cache  );
    REWIND( rs->pack   );
    REWIND( rs->open   );
    REWIND( rs->fail   );
    REWIND( rs->pass   );
    REWIND( rs->source );
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

    for(List* l = rs->source; l; l = l->next){
        Source* s = (Source*)l->value;
        REWIND(s->lines);
        printf("SOURCE %s START\n", s->lang);
        for(List* ll = s->lines; ll; ll = ll->next){
            printf("  %s", (char*)ll->value);
        }
        printf("SOURCE END\n");
    }
}
