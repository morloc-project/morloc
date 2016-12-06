%{

#include <stdio.h>
#include <stdlib.h>
#include <stding.h>

#include "rats.tab.h"
#include "lex.yy.h"

int yylex(void);
void yyerror (char* s);
char* make_label(char*);
char* e_link(char* a, char* b);
char* e_join(char* a, char* b);
char* e_emit(char* a);

%}

%define api.value.type {char*}
%token VAR EOL

%left DEP 

%%

input
  : %empty
  | input line
;

line
  : EOL
  | path EOL
;

path
  : source DEP var { $$ = e_link($3, $1); }
  | path DEP var  { $$ = e_link($3, $1); }
;

source
  : var           
  | source source { $$ = e_join($1, $2); }
;

var
  : VAR { $$ = e_emit($1); }

%%

char* make_label(char* a){
    static int node_idx = 0;
    char* a_star = (char*)malloc(64 * sizeof(char));
    sprintf(a_star, "%s_%d", a, node_idx);
    ++node_idx;
    return(a_star);
}

char* e_link(char* a, char* b){
    printf("link %s %s\n", a, b);
    return a;
}

char* e_join(char* a, char* b){
    char* out = (char*)malloc(128 * sizeof(char));
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
