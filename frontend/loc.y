%{
    #include <stdio.h>
    #include "lex.yy.h"

    void yyerror(const char *);
%}

%code requires{
#include "lil.h"
#include "build.h"
#include "type.h"
#include "ws.h"
#include "lhs.h"
Ws* global_table;
}

%define api.value.type union 

%token <W*> IDENTIFIER /* K_LABEL */
%token <W*> COMPOSON   /* C_MANIFOLD | C_GRPREF | C_POSITIONAL */
%token <W*> SELECTION  /* K_LIST */

%token <W*> STR NAME PRIMITIVE VARIABLE TYPE OTYPE /* P_STRING */

%token COUPLE AS ARROW

%token SECTION_EFFECT
%token SECTION_HOOK
%token SECTION_CACHE
%token SECTION_PATH
%token SECTION_CHECK
%token SECTION_OPEN
%token SECTION_PACK
%token SECTION_PASS
%token SECTION_FAIL
%token SECTION_ALIAS
%token SECTION_LANG
%token SECTION_DOC
%token SECTION_EXPORT
%token SECTION_SOURCE
%token SECTION_ARG
%token SECTION_TYPE
%token SECTION_ONTOLOGY

%type <Ws*> section composition s_path

%type <Ws*> s_effect
%type <Ws*> s_hook
%type <Ws*> s_cache
%type <Ws*> s_check
%type <Ws*> s_open
%type <Ws*> s_pack
%type <Ws*> s_pass
%type <Ws*> s_fail
%type <Ws*> s_alias
%type <Ws*> s_lang
%type <Ws*> s_doc
%type <Ws*> s_arg

%type <Ws*> s_export
%type <Ws*> s_type
%type <Ws*> s_ontology
%type <Ws*> s_source

%type <Ws*> type
%type <Ws*> ontology construct

%type <W*>  argument   /* P_ARGUMENT:V_COUPLET */
%type <Ws*> array list /* P_WS of P_STRING     */

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
    | s_hook
    | s_cache
    | s_check
    | s_open
    | s_pack
    | s_pass
    | s_fail
    | s_alias
    | s_lang
    | s_doc
    | s_export
    | s_type
    | s_ontology
    | s_source
    | s_arg


/* ======================================= */

s_path
    : SECTION_PATH { $$ = NULL; }
    | s_path IDENTIFIER COUPLE composition {
        Couplet* c = couplet_new($2, w_new(P_WS, $4));
        W* w = w_new(T_PATH, c);
        $$ = ws_add($1, w);
    }

s_effect
    : SECTION_EFFECT { $$ = NULL; }
    | s_effect SELECTION COUPLE composition {
        Couplet* c = couplet_new($2, w_new(P_WS, $4));
        W* w = w_new(T_EFFECT, c);
        $$ = ws_add($1, w);
    }

s_hook
    : SECTION_HOOK { $$ = NULL; }
    | s_hook SELECTION COUPLE composition {
        Couplet* c = couplet_new($2, w_new(P_WS, $4));
        W* w = w_new(T_HOOK, c);
        $$ = ws_add($1, w);
    }

s_check
    : SECTION_CHECK { $$ = NULL; }
    | s_check SELECTION COUPLE composition {
        Couplet* c = couplet_new($2, w_new(P_WS, $4));
        W* w = w_new(T_CHECK, c);
        $$ = ws_add($1, w);
    }

s_fail
    : SECTION_FAIL { $$ = NULL; }
    | s_fail SELECTION COUPLE composition {
        Couplet* c = couplet_new($2, w_new(P_WS, $4));
        W* w = w_new(T_FAIL, c);
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
        Ws* ws = ws_join(g_ws($1->last), g_ws($2->head));
        s_ws($1->last, ws);
    }
    | composition '.' composition {
        $$ = ws_join($1, $3);
    }


/* ======================================= */

s_cache
    : SECTION_CACHE { $$ = NULL; }
    | s_cache SELECTION COUPLE VARIABLE {
        Couplet* c = couplet_new($2, $4);
        W* w = w_new(T_CACHE, c);
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

s_alias
    : SECTION_ALIAS { $$ = NULL; }
    | s_alias SELECTION COUPLE VARIABLE {
        Couplet* c = couplet_new($2, $4);
        W* w = w_new(T_ALIAS, c);
        $$ = ws_add($1, w);
    }

s_lang
    : SECTION_LANG { $$ = NULL; }
    | s_lang SELECTION COUPLE VARIABLE {
        Couplet* c = couplet_new($2, $4);
        W* w = w_new(T_LANG, c);
        $$ = ws_add($1, w);
    }

s_doc
    : SECTION_DOC { $$ = NULL; }
    | s_doc SELECTION COUPLE STR {
        Couplet* c = couplet_new($2, $4);
        W* w = w_new(T_DOC, c);
        $$ = ws_add($1, w);
    }


 /* ======================================= */

s_export
    : SECTION_EXPORT { $$ = NULL; }
    | s_export STR {
        Label* e = label_new_set(g_string($2), NULL);
        $$ = ws_add_val($$, T_EXPORT, e);
    }
    | s_export AS STR {
        g_label($$->last)->label = g_string($3);
    }


 /* ======================================= */

s_source
  : SECTION_SOURCE STR {
    Ws* ws = ws_new(w_new(P_STRING, ""));
    Couplet* c = couplet_new($2, w_new(P_WS, ws));
    W* w = w_new(T_SOURCE, c);
    $$ = ws_new(w);
  }
  | s_source STR {
    s_ws(g_rhs($1->head), ws_add(g_ws(g_rhs($1->head)), $2));
    $$ = $1;
  }


 /* ======================================= */

s_type
  : SECTION_TYPE { $$ = NULL; }
  | s_type NAME COUPLE type {
    Couplet* c = couplet_new($2, w_new(P_WS, $4)); 
    W* w = w_new(T_TYPE, c);
    $$ = ws_add($$, w);
  }

type
  : TYPE { $$ = ws_new($1); }
  | '(' type ')' { $$ = ws_new(w_new(P_WS, $2)); }
  | type ARROW type { $$ = ws_join($1, $3); }


 /* ======================================= */

s_ontology
  : SECTION_ONTOLOGY { $$ = NULL; }
  | s_ontology NAME COUPLE ontology {
    Couplet* c = couplet_new($2, w_new(P_WS, $4)); 
    W* w = w_new(T_ONTOLOGY, c);
    $$ = ws_add($$, w);
  }

ontology
  : construct { $$ = ws_new(w_new(P_WS, $1)); }
  | ontology '|' construct { $$ = ws_add($1, w_new(P_WS, $3)); }

construct
  : OTYPE { $$ = ws_new($1); }
  | construct OTYPE { $$ = ws_add($1, $2); }


 /* ======================================= */

s_arg
  : SECTION_ARG { $$ = NULL; }
  | s_arg SELECTION COUPLE argument {
    Couplet* c = couplet_new($2, $4); 
    W* w = w_new(T_ARGUMENT, c);
    $$ = ws_add($$, w);
  }
  | s_arg argument {
    if($$){
        Couplet* c = couplet_new(g_lhs($$->last), $2); 
        W* w = w_new(T_ARGUMENT, c);
        $$ = ws_add($$, w);
    } else {
        fprintf(stderr, "ERROR: missing path in argument declaration\n");
    }
  }
argument
  : NAME '=' PRIMITIVE {
    W* w = w_new(P_WS, ws_new($3));
    $$ = w_new(P_ARGUMENT, couplet_new($1, w));
  }
  | PRIMITIVE {
    W* w = w_new(P_WS, ws_new($1));
    W* s = w_new(P_STRING, "");
    $$ = w_new(P_ARGUMENT, couplet_new(s, w));
  }
  | NAME '=' array {
    $$ = w_new(P_ARGUMENT, couplet_new($1, w_new(P_WS, $3)));
  }
array
  : '[' list ']' { $$ = $2;   }
  | '['      ']' { $$ = NULL; }
list
  : PRIMITIVE          { $$ = ws_new($1);     }
  | list ',' PRIMITIVE { $$ = ws_add($1, $3); }

%%

void yyerror(char const *s){
    fprintf(stderr, "ERROR: %s\n", s);
}
