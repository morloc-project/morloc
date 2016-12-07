%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "rat.tab.h"
#include "lex.yy.h"

int yylex(void);
void yyerror (char* s);
char* get_str();
char* make_label(char*);
char* e_link(char* a, char* b);
char* e_join(char* a, char* b);
char* e_emit(char* a);

%}

%define api.value.type {char*}
%token VAR EOS

%token BUILTIN GROUP KEYWORD

%token COMPOSE EQUAL COUPLE LABEL

%token LBRK RBRK SEP

%token SECTION

%left DEP 

%%

input
  : %empty
  | input section
;

section
  : SECTION { printf("  %s\n", $1); }
  | section statement
;


statement
  : EOS
  | path   EOS
  | tokens EOS { printf("%s\n", $1); }
;

path
  : source DEP var { $$ = e_link($3, $1); }
  | path   DEP var { $$ = e_link($3, $1); }
;

source
  : var        { $$ = $1; }
  | source var { $$ = e_join($1, $2); }
;

var
  : VAR { $$ = e_emit($1); }

tokens
  : token        { $$ = $1; } 
  | tokens token { $$ = get_str(); sprintf($$, "%s %s", $1, $2); } 

token
  : COMPOSE { $$ = "COMPOSE"; }
  | EQUAL   { $$ = "EQUAL";   }
  | COUPLE  { $$ = "COUPLE";  }
  | LABEL   { $$ = "LABEL";   }
  | LBRK    { $$ = "LBRK";    }
  | RBRK    { $$ = "RBRK";    }
  | SEP     { $$ = "SEP";     }
  | KEYWORD { $$ = "KEYWORD"; }
  | BUILTIN { $$ = "BUILTIN"; }
  | GROUP   { $$ = "GROUP";   }

%%

char* get_str(){
    char* a = (char*)malloc(128 * sizeof(char));
    return a;
}

char* make_label(char* a){
    static int node_idx = 0;
    char* a_star = get_str();
    sprintf(a_star, "%s_%d", a, node_idx);
    ++node_idx;
    return(a_star);
}

char* e_link(char* a, char* b){
    printf("link %s %s\n", a, b);
    return a;
}

char* e_join(char* a, char* b){
    char* out = get_str();
    sprintf(out, "%s %s", a, b);
    return out;
}

char* e_emit(char* a){
    char* a_inst = make_label(a);
    printf("emit %s %s\n", a, a_inst);
    return a_inst;
}

void yyerror (char* s){
  printf ("%s\n", s);
}

int main (void){
  yyparse();
}
