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

%token VAR INT DBL STR EOS

%token BUILTIN GROUP

%token WITH USING SPLIT ON MERGE

%token LBRK RBRK LPAR RPAR SEP

%token SECTION_RUN
%token SECTION_COMPOSE
%token SECTION_ALIAS
%token SECTION_ARG
%token SECTION_CACHE
%token SECTION_CHECK
%token SECTION_EFFECT
%token SECTION_PACK
%token SECTION_OPEN
%token SECTION_FAIL
%token SECTION_PASS
%token SECTION_LOOP

%left COUPLE
%left COMPOSE
%left DEP 
%left EQUAL
%left LABEL

%%

/* a rat program consists of a series of sections*/
input
  : %empty
  | input section
;

section
  : section_run
  | section_compose
  | section_alias
  | section_arg
  | section_cache
  | section_check
  | section_effect
  | section_pack
  | section_open
  | section_fail
  | section_pass
  | section_loop


/* sections have their own strict rules on their contents */
section_run
  : SECTION_RUN
  | section_run named_path

section_compose
  : SECTION_COMPOSE
  | section_compose var EQUAL composition

section_alias
  : SECTION_ALIAS
  | section_alias var EQUAL var

section_check
  : SECTION_CHECK
  | section_check path

section_effect
  : SECTION_EFFECT
  | section_effect path

section_arg
  : SECTION_ARG
  | section_arg args_couplet

section_cache
  : SECTION_CACHE
  | section_cache cache_couplet

section_pack
  : SECTION_PACK
  | section_pack pack_couplet

section_open
  : SECTION_OPEN
  | section_open open_couplet

section_fail
  : SECTION_FAIL
  | section_fail fail_couplet

section_pass
  : SECTION_PASS
  | section_pass pass_couplet

section_loop
  : SECTION_LOOP
  | section_loop loop_spec


/* definitions for groups used directly by the section group above */
named_path
  : var COUPLE path
;

path
  : source DEP var
  | path   DEP var
  | LPAR path RPAR
;
source
  : var
  | source var
;

composition
  : var COMPOSE var
  | composition COMPOSE var
;

args_couplet
  : var COUPLE arg 
  | args_couplet SEP arg
;

cache_couplet
  : list COUPLE var
;

pack_couplet
  : list COUPLE var
;

open_couplet
  : list COUPLE var
;

fail_couplet
  : list COUPLE var
;

pass_couplet
  : list COUPLE var
;

loop_spec
  : WITH var
  | loop_spec SPLIT USING var 
  | loop_spec arg_split
  | loop_spec MERGE USING var
;


/* groups used in defining argument lists */
arg_split
  : SPLIT var ON arg
  | arg_split var ON arg
;

arg
  : var EQUAL element
;

element
  : var
  | primitive
  | array

array
  : LBRK list RBRK
;

list
  : element SEP element
  | list SEP element

var
  : VAR
;

primitive
  : INT
  | DBL
  | STR 
;

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
