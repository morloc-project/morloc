%{
    #include <stdio.h>
    #include "lex.yy.h"

    void yyerror(const char *);
%}

%code requires{
#include "lil.h"
#include "build.h"
Table* global_table;
}

%define api.value.type union 

%token <char*> VARIABLE
%token <Selection*> SELECTION

%token <Id*> IDENTIFIER
%token <Entry*> COMPOSON

%token COUPLE

%token SECTION_EFFECT
%token SECTION_PATH

%type <Table*> section
%type <Table*> composition

%type <Table*> s_effect
%type <Table*> s_path

%left '.'
%precedence CONCAT

%%

input
    : section { global_table = $1;}
    | input section { global_table = table_join(global_table, $2); }

section
    : s_path
    | s_effect

s_path
    : SECTION_PATH { $$ = NULL; }
    | s_path IDENTIFIER COUPLE composition {
        Entry* e = entry_new($2, T_PATH, $4);
        $$ = table_add($1, e);
    }

s_effect
    : SECTION_EFFECT { $$ = NULL; }
    | s_effect SELECTION COUPLE VARIABLE {
        Effect* effect = effect_new($2, $4);
        Entry* e = entry_new(NULL, T_EFFECT, effect); 
        $$ = table_add($1, e);
    }

composition
    : COMPOSON {
        Entry* c = entry_new(NULL, C_COMPOSON, table_new($1));
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
