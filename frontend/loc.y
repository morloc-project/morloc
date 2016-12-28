%{
    #include <stdio.h>
    #include "lex.yy.h"

    void yyerror(const char *);
%}

%code requires{
#include "lil.h"
Table* table;
}

%define api.value.type union 

%token <Id*> VARIABLE
%token <Selection*> SELECTION

%type <Entry*> exp
%type <Table*> composition

%token EFFECT COMPOSITION

%left '.'
%precedence CONCAT

%%

input
    : exp { table = table_new($1); }
    | input exp { table = table_add(table, $2); }

exp
    : COMPOSITION VARIABLE '=' composition {
        $$ = entry_new($2, T_PATH, $4);
    }
    | EFFECT SELECTION '=' VARIABLE {
        if($4->label){
            fprintf(stderr, "WARNING: labels are ignored on rhs of effect\n");
        }
        Effect* effect = effect_new($2, $4->name);
        $$ = entry_new(NULL, T_EFFECT, effect); 
    }

composition
    : VARIABLE {
        Manifold* m = manifold_new();
        m->function = $1->name;
        Entry* e = entry_new($1, C_MANIFOLD, m);
        Entry* c = entry_new(NULL, C_COMPOSON, table_new(e));
        $$ = table_new(c);
    }
    | '(' composition ')' { $$ = $2; }
    | composition composition %prec CONCAT {
        Entry* e = entry_new(NULL, C_NEST, $2);
        $1->tail->value.table = table_add($1->tail->value.table, e);
        $$ = $1;
    }
    | composition '.' composition { $$ = table_join($1, $3); }

%%

void yyerror(char const *s){
    fprintf(stderr, "Oh no Mr. Wizard! %s\n", s);
}
