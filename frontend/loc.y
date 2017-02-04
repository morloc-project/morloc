%{
    #include <stdio.h>
    #include "lex.yy.h"
    #include "ws.h"

    void yyerror(const char *);
    Ws* w_make_couplet(Ws* ws, W* lhs, W*   op, W* rhs, Class cls);
    Ws* c_make_couplet(Ws* ws, W* lhs, char op, W* rhs, Class cls);
    Class hook_type(W* w);
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

%token <W*> STR NAME NAMES PRIMITIVE VARIABLE TYPE OTYPE /* P_STRING */
%type  <W*> maybe_variable maybe_str

%token AS ARROW RESET
%token <W*> COUPLE

%token <W*> SECTION_HOOK /* unforgivable kludge alert */

%token SECTION_CACHE
%token SECTION_PATH
%token SECTION_CHECK
%token SECTION_FAIL
%token SECTION_ALIAS
%token SECTION_LANG
%token SECTION_DOC
%token SECTION_EXPORT
%token SECTION_SOURCE
%token SECTION_ARG
%token SECTION_TYPE
%token SECTION_ONTOLOGY

%type <Ws*> section composition maybe_composition

%type <Ws*> s_path
%type <Ws*> s_hook
%type <Ws*> s_cache
%type <Ws*> s_check
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

%type <W*>  maybe_argument /* P_ARGUMENT:V_COUPLET */
%type <Ws*> array list     /* P_WS of P_STRING     */

%left '.'
%precedence CONCAT
%precedence '&'

%%

input
    : section { global_table = $1;}
    | input section { global_table = ws_join(global_table, $2); }

section
    : s_path
    | s_hook { $$ = ws_tail($$); /* see KLUDGE NOTE below */ } 
    | s_cache
    | s_check
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
    | SECTION_PATH composition {
        Label* l = label_new_set("default", NULL);
        W* label = w_new(K_LABEL, l);
        $$ = c_make_couplet(NULL, label, '=', w_new(P_WS, $2), T_PATH);
    }
    | s_path IDENTIFIER COUPLE composition {
        $$ = w_make_couplet($1, $2, $3, w_new(P_WS, $4), T_PATH);
    }

/* KLUDGE NOTE: hook is the one section that is comprised of multiple types:
 * T_H0, T_H1, ... T_H9. These correspond to functions that are called within
 * a manifold (see notes in R-manifold.R). There is not natural place to put
 * this information in a Ws. So I add a temporary first element to the list.
 * It is removed above.
 */
s_hook
    : SECTION_HOOK { $$ = ws_new($1); }
    | s_hook SELECTION COUPLE maybe_composition {
        Class c = hook_type($1->head);
        $$ = w_make_couplet($1, $2, $3, w_new(P_WS, $4), c);
    }

s_check
    : SECTION_CHECK { $$ = NULL; }
    | s_check SELECTION COUPLE maybe_composition {
        $$ = w_make_couplet($1, $2, $3, w_new(P_WS, $4), T_CHECK);
    }

s_fail
    : SECTION_FAIL { $$ = NULL; }
    | s_fail SELECTION COUPLE maybe_composition {
        $$ = w_make_couplet($1, $2, $3, w_new(P_WS, $4), T_FAIL);
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
    : SECTION_CACHE { $$ = NULL; }
    | s_cache SELECTION COUPLE maybe_variable {
        $$ = w_make_couplet($1, $2, $3, $4, T_CACHE);
    }

s_alias
    : SECTION_ALIAS { $$ = NULL; }
    | s_alias SELECTION COUPLE maybe_variable {
        $$ = w_make_couplet($1, $2, $3, $4, T_ALIAS);
    }

s_lang
    : SECTION_LANG { $$ = NULL; }
    | s_lang SELECTION COUPLE maybe_variable {
        $$ = w_make_couplet($1, $2, $3, $4, T_LANG);
    }

s_doc
    : SECTION_DOC { $$ = NULL; }
    | s_doc SELECTION COUPLE maybe_str {
        $$ = w_make_couplet($1, $2, $3, $4, T_DOC);
    }

maybe_variable
    : VARIABLE
    | RESET { $$ = NULL; }

maybe_str
    : STR
    | RESET { $$ = NULL; }


 /* ======================================= */

s_export
    : SECTION_EXPORT { $$ = NULL; }
    | s_export PATH {
        $$ = c_make_couplet($1, $2, '=', NULL, T_EXPORT);
    }
    | s_export PATH AS VARIABLE {
        $$ = c_make_couplet($1, $2, '=', $4, T_EXPORT);
    }


 /* ======================================= */

s_source
  : SECTION_SOURCE STR {
    Ws* ws = ws_new(w_new(P_STRING, ""));
    $$ = c_make_couplet(NULL, $2, '=', w_new(P_WS, ws), T_SOURCE);
  }
  | s_source STR {
    s_ws(g_rhs($1->head), ws_add(g_ws(g_rhs($1->head)), $2));
    $$ = $1;
  }


 /* ======================================= */

s_type
  : SECTION_TYPE { $$ = NULL; }
  | s_type NAMES COUPLE type {
    $$ = w_make_couplet($1, $2, $3, w_new(P_WS, $4), T_TYPE); 
  }

type
  : TYPE { $$ = ws_new($1); }
  | '(' type ')' {
    W* rhs = w_new(P_WS, $2);
    $$ = c_make_couplet(NULL, w_new(P_STRING, "function"), '=', rhs, P_TYPE);
  }
  | '[' type ']' {
    W* rhs = w_new(P_WS, $2);
    $$ = c_make_couplet(NULL, w_new(P_STRING, "array"), '=', rhs, P_TYPE);
  }
  | type ARROW type { $$ = ws_join($1, $3); }


 /* ======================================= */

s_ontology
  : SECTION_ONTOLOGY { $$ = NULL; }
  | s_ontology NAMES COUPLE ontology {
    $$ = w_make_couplet($1, $2, $3, w_new(P_WS, $4), T_ONTOLOGY); 
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
  | s_arg SELECTION COUPLE maybe_argument {
    $$ = w_make_couplet($1, $2, $3, $4, T_ARGUMENT);
  }
  | s_arg maybe_argument {
    if($$){
        char op = g_couplet($$->head)->op;
        // If multiple arguments are assigned together, the first resets the
        // argument list ('='), but subsequent ones are appended ('+').
        op = op == '=' ? '+' : op;
        $$ = c_make_couplet($1, g_lhs($$->last), op, $2, T_ARGUMENT);
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
    warn("ERROR: %s\n", s);
}


Ws* w_make_couplet(Ws* ws, W* lhs, W* op, W* rhs, Class cls){
    s_lhs(op, lhs);
    s_rhs(op, rhs);
    op->cls = cls;
    return ws_add(ws, op);
}

Ws* c_make_couplet(Ws* ws, W* lhs, char op, W* rhs, Class cls){
    Couplet* c = couplet_new(lhs, rhs, op); 
    W* w = w_new(cls, c);
    return ws_add(ws, w);
}

Class hook_type(W* w){
    switch(g_string(w)[0]){
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
