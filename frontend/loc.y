%{
    #include <stdio.h>
    #include "lex.yy.h"

    void yyerror(const char *);
%}

%code requires{
#include "lil.h"
#include "build.h"
#include "ws.h"
#include "lhs.h"
Ws* global_table;
}

%define api.value.type union 

%token <W*> IDENTIFIER COMPOSON VARIABLE SELECTION

%token COUPLE

%token SECTION_EFFECT
%token SECTION_PATH

%type <Ws*> section composition s_effect s_path

%left '.'
%precedence CONCAT
%precedence '&'

%%

input
    : section { global_table = $1;}
    | input section { global_table = ws_join(global_table, $2); }

section
    : s_path
    | s_effect

s_path
    : SECTION_PATH { $$ = NULL; }
    | s_path IDENTIFIER COUPLE composition {
        Couplet* c = couplet_new($2, w_new(P_WS, $4));
        W* w = w_new(T_PATH, c);
        $$ = ws_add($1, w);
    }

s_effect
    : SECTION_EFFECT { $$ = NULL; }
    | s_effect SELECTION COUPLE VARIABLE {
        Couplet* c = couplet_new($2, $4);
        W* w = w_new(T_EFFECT, c); 
        $$ = ws_add($1, w);
    }

composition
    : COMPOSON {
        Ws* ws = ws_new($1);
        W* w = w_new(C_COMPOSON, ws);
        $$ = ws_new(w);
    }
    | '(' composition ')' {
        W* n = w_new(C_NEST, $2);
        W* c = w_new(C_COMPOSON, ws_new(n));
        $$ = ws_new(c);
    }
    | '&' COMPOSON {
        W* e = w_new(C_DEREF, ws_new($2));
        W* c = w_new(C_COMPOSON, ws_new(e));
        $$ = ws_new(c);
    }
    | '&' '(' composition ')' {
        W* e = w_new(C_NEST, $3);
        W* d = w_new(C_DEREF, ws_new(e));
        W* n = w_new(C_COMPOSON, ws_new(d));
        $$ = ws_new(n);
    }
    | composition composition %prec CONCAT {
        Ws* ws = ws_join(g_ws($1->tail), g_ws($2->head));
        s_ws($1->tail, ws);
    }
    | composition '.' composition {
        $$ = ws_join($1, $3);
    }


%%

void yyerror(char const *s){
    fprintf(stderr, "Oh no Mr. Wizard! %s\n", s);
}
