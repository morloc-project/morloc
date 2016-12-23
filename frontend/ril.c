#include "ril.h"

RatStack* new_RatStack(){
     RatStack* rs = (RatStack*)calloc(1,sizeof(RatStack));
     rs->export = new_NamedList();
     rs->doc    = new_NamedList();
     rs->alias  = new_NamedList();
     rs->cache  = new_NamedList();
     rs->pack   = new_NamedList();
     rs->open   = new_NamedList();
     rs->fail   = new_NamedList();
     rs->pass   = new_NamedList();
     rs->source = new_NamedList();

     rs->ontology = new_List();
     rs->type     = new_List();

     rs->path   = new_NamedList();
     rs->check  = new_NamedList();
     rs->effect = new_NamedList();

     return rs;
}

void rewind_RatStack(RatStack* rs){
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

void print_couplet(NamedList* l, char* cmd){
    if(l && l->value){
        for( ; l; l = l->next ){
            printf("%s %s %s\n", cmd, l->name->name, l->value);
        }
    }
}

void print_list(List* l, char* cmd){
    if(l && l->value){
        for( ; l; l = l->next ){
            printf("%s %s\n", cmd, l->value);
        }
    }
}

void print_composition_r(List* l, int depth){
    REWIND(l);
    int k = 1;
    for( ; l; l = l->next){
        for(int i = depth + 1; i > 0; i--) { printf(" -"); }
        printf(" %d\n", k++);
        List* ll = (List*)l->value;
        REWIND(ll);
        for( ; ll; ll = ll->next){ 
            Composon* c = (Composon*)ll->value; 
            for(int i = (2*depth - 1) + 2; i > 0; i--) { printf(" -"); }
            switch(c->type){
                case C_VARIABLE:
                    printf(" VARIABLE:(%s,%d)\n", c->value.name, c->manifold->uid);
                    break;
                case C_POSITIONAL:
                    printf(" POS\n");
                    break;
                case C_GROUP:
                    printf(" GROUP\n");
                    break;
                case C_CONDITIONAL:
                    printf(" CONDITIONAL\n");
                    break;
                case C_NEST:
                    printf(" NEST\n");
                    print_composition_r((List*)c->value.nest, depth+1);
                    break;
                default:
                    fprintf(stderr, "Invalid composon type\n");
                    exit(EXIT_FAILURE);
            }
        }
    }
}

void print_paths(NamedList* p, char* cmd){
    printf("%s\n", cmd);
    NamedList* l = p;
    if(l && l->value){
        REWIND(l);
        for( ; l; l = l->next){
            printf(" %s\n", l->name->name);
            print_composition_r((List*)l->value, 0);
        }
    }
}

void set_manifold_value(char* name, Label* path, size_t index){
    /* stub */
}

void print_RIL(RatStack* rs){

    rewind_RatStack(rs);

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
