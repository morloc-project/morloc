#include "ril.h"

RatStack* ratstack_new(){
     RatStack* rs = (RatStack*)calloc(1,sizeof(RatStack));
     rs->export = namedlist_new();
     rs->doc    = namedlist_new();
     rs->alias  = namedlist_new();
     rs->cache  = namedlist_new();
     rs->pack   = namedlist_new();
     rs->open   = namedlist_new();
     rs->fail   = namedlist_new();
     rs->pass   = namedlist_new();
     rs->source = namedlist_new();

     rs->ontology = list_new();
     rs->type     = list_new();

     rs->path   = namedlist_new();
     rs->check  = namedlist_new();
     rs->effect = namedlist_new();

     return rs;
}

void ratstack_rewind(RatStack* rs){
    REWIND( rs->export   );
    REWIND( rs->doc      );
    REWIND( rs->alias    );
    REWIND( rs->cache    );
    REWIND( rs->pack     );
    REWIND( rs->open     );
    REWIND( rs->fail     );
    REWIND( rs->pass     );
    REWIND( rs->source   );
    REWIND( rs->ontology );
    REWIND( rs->type     );
}

void set_manifold_value(char* name, Label* path, size_t index){
    /* stub */
}

void print_RIL(RatStack* rs){

    ratstack_rewind(rs);

    print_couplet(rs->export,   "EXPORT");
    print_couplet(rs->doc,      "DOC");
    print_couplet(rs->alias,    "ALIAS");
    print_couplet(rs->cache,    "CACHE");
    print_couplet(rs->pack,     "PACK");
    print_couplet(rs->open,     "OPEN");
    print_couplet(rs->fail,     "FAIL");
    print_couplet(rs->pass,     "PASS");
    print_couplet(rs->source,   "SOURCE");

    print_list(rs->ontology, "ONTOLOGY");
    print_list(rs->type,     "TYPE");

    print_paths(rs->path, "PATH");
    print_paths(rs->effect, "EFFECT");
    print_paths(rs->check, "CHECK");
}
