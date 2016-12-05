%{

#include <stdio.h>
#include <stdlib.h>

#include "R.tab.h"
#include "lex.yy.h"

int yylex(void);
void yyerror (char* s);

char* get_str();
char* join(char* a, char* b);

%}

%define api.value.type {char*}
%token EMIT LINK PAR EOL

%%

input
  : %empty
  | input line
;

line
  : cmd { printf("%s\n", $1); }
;

cmd
  : emit
  | link
;

emit
  : EMIT PAR PAR EOL { $$ = get_str(); sprintf($$, "%s <- %s()\n", $3, $2); }
;

link
  : LINK PAR PAR { $$ = get_str(); sprintf($$, "%s <- link(%s", $2, $3); }
  | link PAR { $$ = join($1, $2); }
  | link EOL { $$ = get_str(); sprintf($$, "%s)\n", $1); }
;

%%

char* get_str(){
    char* c = (char*)malloc(128 * sizeof(char));
    return c;
}

char* join(char* a, char* b){
    char* c = get_str();
    sprintf(c, "%s, %s", a, b); 
    return c;
}

void yyerror (char* s){
  printf ("%s\n", s);
}

int main (void){
  yyparse();
}
