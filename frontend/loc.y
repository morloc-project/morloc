%{
    #include <stdio.h>
    #include "lex.yy.h"
    #include "bufstack.h"
    #include "ws.h"

    void yyerror(const char *);
    void w_make_couplet(W* w, W* lhs, W*   op, W* rhs, Class cls);
    void c_make_couplet(W* w, W* lhs, char op, W* rhs, Class cls);
    Class hook_type(char*);
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
%token <W*> COMPOSON   /* C_MANIFOLD | C_GRPREF | C_ARGREF | C_POSITIONAL */
%token <W*> SELECTION  /* K_LIST */
%token <W*> PATH       /* K_PATH */

%token <W*> STR NAME NAMES PRIMITIVE VARIABLE TYPE /* P_STRING */
%type  <W*> maybe_variable maybe_str

%token AS ARROW RESET
%token <W*> COUPLE

%token <W*> SECTION_HOOK
%token <W*> SECTION_CACHE
%token <W*> SECTION_PATH
%token <W*> SECTION_ASSERT
%token <W*> SECTION_FAIL
%token <W*> SECTION_ALIAS
%token <W*> SECTION_LANG
%token <W*> SECTION_DOC
%token <W*> SECTION_EXPORT
%token <W*> SECTION_SOURCE
%token <W*> SECTION_ARG
%token <W*> SECTION_TYPE
%token <W*> SECTION_ONTOLOGY

%type <Ws*> composition maybe_composition

%type <W*> section

%type <W*> s_path
%type <W*> s_hook
%type <W*> s_cache
%type <W*> s_assert
%type <W*> s_fail
%type <W*> s_alias
%type <W*> s_lang
%type <W*> s_doc
%type <W*> s_arg

%type <W*> s_export
%type <W*> s_type
%type <W*> s_ontology
%type <W*> s_source

%type <W*> type
%type <Ws*> ontology construct

%type <W*>  maybe_argument /* P_ARGUMENT:V_COUPLET */
%type <Ws*> array list     /* P_WS of P_STRING     */

%left '.'
%precedence CONCAT
%precedence '&'

%%

input
    : section { global_table = ws_new($1);}
    | input section { global_table = ws_add(global_table, $2); }

section
    : s_path
    | s_hook
    | s_cache
    | s_assert
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
    : SECTION_PATH { $$ = $1; }
    | SECTION_PATH composition {
        Label* l = label_new_set("default", NULL, NULL);
        W* label = w_new(K_LABEL, l);
        c_make_couplet($1, label, '=', w_new(P_WS, $2), T_PATH);
    }
    | s_path IDENTIFIER COUPLE composition {
        w_make_couplet($1, $2, $3, w_new(P_WS, $4), T_PATH);
    }

s_hook
    : SECTION_HOOK { $$ = $1; }
    | s_hook SELECTION COUPLE maybe_composition {
        Class c = hook_type(g_section(g_lhs($1))->name);
        w_make_couplet($1, $2, $3, w_new(P_WS, $4), c);
    }

s_assert
    : SECTION_ASSERT { $$ = $1; }
    | s_assert SELECTION COUPLE maybe_composition {
        w_make_couplet($1, $2, $3, w_new(P_WS, $4), T_ASSERT);
    }

s_fail
    : SECTION_FAIL { $$ = $1; }
    | s_fail SELECTION COUPLE maybe_composition {
        w_make_couplet($1, $2, $3, w_new(P_WS, $4), T_FAIL);
    }

maybe_composition
    : composition { $$ = $1;   }
    | RESET       { $$ = NULL; }

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
        W* d = w_new(C_DEREF, $3);
        W* n = w_new(C_COMPOSON, ws_new(d));
        $$ = ws_new(n);
    }
    | composition composition %prec CONCAT {
        $1->last = wws_join($1->last, $2->head);
    }
    | composition '.' composition {
        $$ = ws_join($1, $3);
    }


/* ======================================= */

s_cache
    : SECTION_CACHE { $$ = $1; }
    | s_cache SELECTION COUPLE maybe_variable {
        w_make_couplet($1, $2, $3, $4, T_CACHE);
    }

s_alias
    : SECTION_ALIAS { $$ = $1; }
    | s_alias SELECTION COUPLE maybe_variable {
        w_make_couplet($1, $2, $3, $4, T_ALIAS);
    }

s_lang
    : SECTION_LANG { $$ = $1; }
    | s_lang SELECTION COUPLE maybe_variable {
        w_make_couplet($1, $2, $3, $4, T_LANG);
    }

s_doc
    : SECTION_DOC { $$ = $1; }
    | s_doc SELECTION COUPLE maybe_str {
        w_make_couplet($1, $2, $3, $4, T_DOC);
    }

maybe_variable
    : VARIABLE
    | RESET { $$ = NULL; }

maybe_str
    : STR
    | RESET { $$ = NULL; }


 /* ======================================= */

s_export
    : SECTION_EXPORT { $$ = $1; }
    | s_export PATH {
        c_make_couplet($1, $2, '=', NULL, T_EXPORT);
    }
    | s_export PATH AS VARIABLE {
        c_make_couplet($1, $2, '=', $4, T_EXPORT);
    }


 /* ======================================= */

s_source
  : SECTION_SOURCE STR { $$ = $1; s_rhs($1, wws_new($2)); }
  | s_source STR {
    s_rhs($1, wws_add(g_rhs($1), $2));
  }


 /* ======================================= */

s_type
  : SECTION_TYPE { $$ = $1; }
  | s_type NAMES COUPLE type {
    w_make_couplet($1, $2, $3, $4, T_TYPE); 
  }

type
  : TYPE         { $$ = wws_new_cls($1, FT_FUNCTION); }
  | '(' type ')' {
    $$ = wws_new_cls($2, FT_FUNCTION);
  }
  | '[' type ']' {
    $2->cls = FT_ARRAY;
    $$ = wws_new_cls($2, FT_FUNCTION);
  }
  | type ARROW type {
    $$ = wws_join($1, $3);
  }
  | type ',' type {
    $$ = wws_join($1, $3);
    $$->cls = FT_TUPLE;
  }


 /* ======================================= */

s_ontology
  : SECTION_ONTOLOGY { $$ = $1; }
  | s_ontology NAMES COUPLE ontology {
    w_make_couplet($1, $2, $3, w_new(P_WS, $4), T_ONTOLOGY); 
  }

ontology
  : construct { $$ = ws_new(w_new(P_WS, $1)); }
  | ontology '|' construct { $$ = ws_add($1, w_new(P_WS, $3)); }

construct
  : TYPE { $$ = ws_new($1); }
  | construct TYPE { $$ = ws_add($1, $2); }


 /* ======================================= */

s_arg
  : SECTION_ARG { $$ = $1; }
  | s_arg SELECTION COUPLE maybe_argument {
    w_make_couplet($1, $2, $3, $4, T_ARGUMENT);
  }
  | s_arg maybe_argument {
    if($$){
        char op = g_couplet(g_ws(g_rhs($$))->head)->op;
        // If multiple arguments are assigned together, the first resets the
        // argument list ('='), but subsequent ones are appended ('+').
        op = op == '=' ? '+' : op;
        c_make_couplet($1, g_lhs(wws_last(g_rhs($$))), op, $2, T_ARGUMENT);
    } else {
        warn("ERROR: missing path in argument declaration\n");
    }
  }
maybe_argument
  : RESET { $$ = NULL; }
  | NAME '=' PRIMITIVE {
    W* w = w_new(P_WS, ws_new($3));
    $$ = w_new(P_ARGUMENT, couplet_new($1, w, '='));
  }
  | PRIMITIVE {
    W* w = w_new(P_WS, ws_new($1));
    W* s = w_new(P_STRING, "");
    $$ = w_new(P_ARGUMENT, couplet_new(s, w, '='));
  }
  | NAME '=' array {
    $$ = w_new(P_ARGUMENT, couplet_new($1, w_new(P_WS, $3), '='));
  }
array
  : '[' list ']' { $$ = $2;   }
  | '['      ']' { $$ = NULL; }
list
  : PRIMITIVE          { $$ = ws_new($1);     }
  | list ',' PRIMITIVE { $$ = ws_add($1, $3); }

%%

void yyerror(char const *s){
    warn("(%s:%d) %s\n", yyfilename, yylineno, s);
}

void w_make_couplet(W* w, W* lhs, W* op, W* rhs, Class cls){
    s_lhs(op, lhs);
    s_rhs(op, rhs);
    op->cls = cls;
    W* wc = wws_add(g_rhs(w), op);
    s_rhs(w, wc);
}

void c_make_couplet(W* w, W* lhs, char op, W* rhs, Class cls){
    Couplet* c = couplet_new(lhs, rhs, op); 
    W* cw = w_new(cls, c);
    s_rhs(w, wws_add(g_rhs(w), cw));
}

Class hook_type(char* name){
    char c;
    if(strcmp(name, "before") == 0){
        c = '4';
    }
    else if(strcmp(name, "after") == 0){
        c = '5';
    }
    else {
        c = name[0];
    }

    switch(c){
        case '0': return T_H0;
        case '1': return T_H1;
        case '2': return T_H2;
        case '3': return T_H3;
        case '4': return T_H4;
        case '5': return T_H5;
        case '6': return T_H6;
        case '7': return T_H7;
        case '8': return T_H8;
        case '9': return T_H9;
        default:
            warn("Illegal hook section, expected @0-9\n");
            return X_NONE;
    }
}
