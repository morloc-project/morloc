%{
    #include "rat.tab.h"
%}

%option outfile="lex.yy.c" header-file="lex.yy.h"

%option noyywrap

comment #.*
ws      [ \t\n\r]+
var     [a-zA-Z_][a-zA-Z0-9_]*
dep     -->
eol     [\r\n]+

compose \.
eos     ;|---+
equal   =
couple  {ws}::{ws}
label   :[a-zA-Z0-9_]+

lbrk    \[
rbrk    \]
sep     ,

section ^(run|compose|alias|arg|cache|check|effect|pack|open|fail|pass|loop):

builtin (memcache|datcache|nocache|true|null|id|call)
keyword (with|split|on|merge)
group   __all__

%%


{comment} { }

{keyword} { yylval = strdup(yytext); return KEYWORD; }
{builtin} { yylval = strdup(yytext); return BUILTIN; }
{group}   { yylval = strdup(yytext); return GROUP;   }
{var}     { yylval = strdup(yytext); return VAR;     }
{section} { yylval = strdup(yytext); return SECTION; }

{dep}     { return DEP; }

{eos}     { return EOS;     }
{compose} { return COMPOSE; }
{equal}   { return EQUAL;   }
{couple}  { return COUPLE;  }
{label}   { return LABEL;   }

{lbrk} { return LBRK; }
{rbrk} { return RBRK; }
{sep}  { return SEP;  }

{ws} { }
