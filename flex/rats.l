STR \'[^']*\'|\"[^"]*\"
EOL \n
NIL NIL
VAL [a-zA-Z_.][a-zA-Z0-9_.]*
DBL [0-9]\.[0-9]+|[1-9][0-9]+\.[0-9]+
INT [0-9]|[1-9][0-9]+

DEP -->
ARW ->
EQU =
DEF ::
SEP ,
LAB :

GATE \?
LPAR \(
RPAR \)
LBRC \{
RBRC \}
LBRK \[
RBRK \]
LDGT <<
RDGT >>
LSLT <
RSLT >

%%

#.*|\/\/.* ;

{DBL} { yylval.fval = atof(yytext);   return DBL; }
{INT} { yylval.ival = atoi(yytext);   return INT; }
{STR} { yylval.sval = strdup(yytext); return STR; }

{VAL}  { yylval.sval = strdup(yytext); return VAL; }
{NIL}  { return NIL; }

{DEP}  { return DEP; }
{ARW}  { return ARW; }
{EQU}  { return EQU; }
{DEF}  { return DEF; }
{SEP}  { return SEP; }
{LAB}  { return LAB; }
{EOL}  { return EOL; }

{GATE} { return GATE; }
{LPAR} { return LPAR; }
{RPAR} { return RPAR; }
{LBRC} { return LBRC; }
{RBRC} { return RBRC; }
{LBRK} { return LBRK; }
{RBRK} { return RBRK; }
{LDGT} { return LDGT; }
{RDGT} { return RDGT; }
{LSLT} { return LSLT; }
{RSLT} { return RSLT; }

%%

int main(int argc, char ** argv){
    yylex(); 
    return 0;
}
