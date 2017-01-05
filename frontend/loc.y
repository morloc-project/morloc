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

%token <W*> STRING
%token <char*> STRLIT

%token COUPLE AS

%token SECTION_EFFECT
%token SECTION_CACHE
%token SECTION_PATH
%token SECTION_CHECK
%token SECTION_OPEN
%token SECTION_PACK
%token SECTION_PASS
%token SECTION_FAIL
%token SECTION_DOC
%token SECTION_EXPORT

%type <Ws*> section composition s_path

%type <Ws*> s_effect
%type <Ws*> s_cache
%type <Ws*> s_check
%type <Ws*> s_open
%type <Ws*> s_pack
%type <Ws*> s_pass
%type <Ws*> s_fail
%type <Ws*> s_doc

%type <Ws*> s_export

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
    | s_cache
    | s_check
    | s_open
    | s_pack
    | s_pass
    | s_fail
    | s_doc
    | s_export

s_path
    : SECTION_PATH { $$ = NULL; }
    | s_path IDENTIFIER COUPLE composition {
        Couplet* c = couplet_new($2, w_new(P_WS, $4));
        W* w = w_new(T_PATH, c);
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


s_effect
    : SECTION_EFFECT { $$ = NULL; }
    | s_effect SELECTION COUPLE VARIABLE {
        Couplet* c = couplet_new($2, $4);
        W* w = w_new(T_EFFECT, c);
        $$ = ws_add($1, w);
    }

s_cache
    : SECTION_CACHE { $$ = NULL; }
    | s_cache SELECTION COUPLE VARIABLE {
        Couplet* c = couplet_new($2, $4);
        W* w = w_new(T_CACHE, c);
        $$ = ws_add($1, w);
    }

s_check
    : SECTION_CHECK { $$ = NULL; }
    | s_check SELECTION COUPLE VARIABLE {
        Couplet* c = couplet_new($2, $4);
        W* w = w_new(T_CHECK, c);
        $$ = ws_add($1, w);
    }

s_open
    : SECTION_OPEN { $$ = NULL; }
    | s_open SELECTION COUPLE VARIABLE {
        Couplet* c = couplet_new($2, $4);
        W* w = w_new(T_OPEN, c);
        $$ = ws_add($1, w);
    }

s_pack
    : SECTION_PACK { $$ = NULL; }
    | s_pack SELECTION COUPLE VARIABLE {
        Couplet* c = couplet_new($2, $4);
        W* w = w_new(T_PACK, c);
        $$ = ws_add($1, w);
    }

s_pass
    : SECTION_PASS { $$ = NULL; }
    | s_pass SELECTION COUPLE VARIABLE {
        Couplet* c = couplet_new($2, $4);
        W* w = w_new(T_PASS, c);
        $$ = ws_add($1, w);
    }

s_fail
    : SECTION_FAIL { $$ = NULL; }
    | s_fail SELECTION COUPLE VARIABLE {
        Couplet* c = couplet_new($2, $4);
        W* w = w_new(T_FAIL, c);
        $$ = ws_add($1, w);
    }

s_doc
    : SECTION_DOC { $$ = NULL; }
    | s_doc SELECTION COUPLE STRING {
        Couplet* c = couplet_new($2, $4);
        W* w = w_new(T_DOC, c);
        $$ = ws_add($1, w);
    }

 /* ======================================= */


s_export
    : SECTION_EXPORT { $$ = NULL; }
    | s_export STRLIT {
        Label* e = label_new_set($2, NULL);
        $$ = ws_add_val($$, T_EXPORT, e);
    }
    | s_export AS STRLIT {
        g_label($$->tail)->label = $3;
    }

%%

void yyerror(char const *s){
    fprintf(stderr, "Oh no Mr. Wizard! %s\n", s);
}
