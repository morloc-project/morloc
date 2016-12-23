#include "composition.h"

Composon* composon_new(ComposonType type){
    Composon* c = (Composon*)malloc(sizeof(Composon));
    c->type = type;
    return c;
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

List* composition_new(Composon* c){
    List* l = list_new();
    List* lc = list_new();
    LADD(lc, c, Composon*);
    LADD(l, lc, List*);
    return l;
}

List* composition_new_nested(List* comp){
    List* over = list_new();
    List* down = list_new();
    Composon* c = composon_new(C_NEST);
    c->value.nest = comp;
    LADD(down, c, Composon*);
    LADD(over, down, List*);
    return over;
}

List* composition_add_down(List* la, List* lb){
      List* a = (List*)la->value;
      List* b = (List*)lb->value;
      JOIN(a, b);
      la->value = a;
      return la;
}

List* composition_compose(List* a, List* b){
      JOIN(a, b);
      return a;
}
