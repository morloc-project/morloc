{
{-# LANGUAGE OverloadedStrings #-}

module Morloc.Frontend.Parser
  ( readProgram
  , readType
  , PState (..)
  , emptyPState
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as DS
import qualified Control.Monad.State.Strict as State
import Morloc.Frontend.Token
import Morloc.Frontend.Lexer (lexMorloc, showLexError)
import Morloc.Frontend.CST
import Morloc.Frontend.Desugar (DState(..), D, ParseError(..), showParseError, desugarProgram, desugarExpr)
import Morloc.Namespace.Prim
import Morloc.Namespace.Type
import Morloc.Namespace.Expr
import qualified Morloc.BaseTypes as BT
}

%name parseProgram program
%name parseTypeOnly type_eof
%name parseExprOnly expr_eof

%tokentype { Located }
%monad { P } { (>>=) } { return }
%error { parseError }
%errorhandlertype explist

-- shift/reduce conflicts, all resolved correctly by shift:
-- - 1 from :: type annotation (infix_expr followed by ::)
-- - 2 from app_expr (./- could start new atom or be operators)
-- - 1 from sig_or_ass (LOWER could be param or start of new decl)
-- - 6 from class_constraints (UPPER could be constraint arg or end of constraint)
-- - 3 from original grammar
-- - 4 from accessor_tail (GDOT/= could continue chain or end it)
%expect 49

%token
  VLBRACE    { Located _ TokVLBrace _ }
  VRBRACE    { Located _ TokVRBrace _ }
  VSEMI      { Located _ TokVSemi _ }
  '('        { Located _ TokLParen _ }
  ')'        { Located _ TokRParen _ }
  '['        { Located _ TokLBracket _ }
  ']'        { Located _ TokRBracket _ }
  '{'        { Located _ TokLBrace _ }
  '}'        { Located _ TokRBrace _ }
  '<'        { Located _ TokLAngle _ }
  '>'        { Located _ TokRAngle _ }
  ','        { Located _ TokComma _ }
  '\\'       { Located _ TokBackslash _ }
  '_'        { Located _ TokUnderscore _ }
  '!'        { Located _ TokBang _ }
  '.'        { Located _ TokDot _ }
  GDOT       { Located _ TokGetterDot _ }
  '='        { Located _ TokEquals _ }
  '::'       { Located _ TokDColon _ }
  '->'       { Located _ TokArrow _ }
  '=>'       { Located _ TokFatArrow _ }
  '<-'       { Located _ TokBind _ }
  '*'        { Located _ TokStar _ }
  '-'        { Located _ TokMinus _ }
  ':'        { Located _ TokColon _ }
  'module'   { Located _ TokModule _ }
  'import'   { Located _ TokImport _ }
  'source'   { Located _ TokSource _ }
  'from'     { Located _ TokFrom _ }
  'where'    { Located _ TokWhere _ }
  'as'       { Located _ TokAs _ }
  'True'     { Located _ TokTrue _ }
  'False'    { Located _ TokFalse _ }
  'type'     { Located _ TokType _ }
  'record'   { Located _ TokRecord _ }
  'object'   { Located _ TokObject _ }
  'table'    { Located _ TokTable _ }
  'class'    { Located _ TokClass _ }
  'instance' { Located _ TokInstance _ }
  'infixl'   { Located _ TokInfixl _ }
  'infixr'   { Located _ TokInfixr _ }
  'infix'    { Located _ TokInfix _ }
  'let'      { Located _ TokLet _ }
  'in'       { Located _ TokIn _ }
  'do'       { Located _ TokDo _ }
  LOWER      { Located _ (TokLowerName _) _ }
  UPPER      { Located _ (TokUpperName _) _ }
  OPERATOR   { Located _ (TokOperator _) _ }
  INTEGER    { Located _ (TokInteger _) _ }
  FLOAT      { Located _ (TokFloat _) _ }
  STRING     { Located _ (TokString _) _ }
  STRSTART   { Located _ (TokStringStart _) _ }
  STRMID     { Located _ (TokStringMid _) _ }
  STREND     { Located _ (TokStringEnd _) _ }
  INTERPOPEN { Located _ TokInterpOpen _ }
  INTERPCLOSE { Located _ TokInterpClose _ }
  EOF        { Located _ TokEOF _ }

%%

--------------------------------------------------------------------
-- Program
--------------------------------------------------------------------

program :: { ([Loc CstExpr], Bool) }
  : modules EOF             { ($1, False) }
  | top_body EOF            { ($1, True) }

-- Standalone entry points with explicit EOF
type_eof :: { TypeU }
  : type EOF                { $1 }

expr_eof :: { Loc CstExpr }
  : expr EOF                { $1 }

modules :: { [Loc CstExpr] }
  : module                   { [$1] }
  | modules module           { $1 ++ [$2] }

module :: { Loc CstExpr }
  : 'module' module_name '(' exports ')' top_body
      { at $1 (CModE $2 $4 $6) }

top_body :: { [Loc CstExpr] }
  : VLBRACE top_decls VRBRACE   { $2 }
  | VLBRACE VRBRACE              { [] }

top_decls :: { [Loc CstExpr] }
  : top_decl                     { $1 }
  | top_decls VSEMI top_decl     { $1 ++ $3 }

top_decl :: { [Loc CstExpr] }
  : import_decl       { [$1] }
  | typedef_decl      { [$1] }
  | typeclass_decl    { [$1] }
  | instance_decl     { [$1] }
  | fixity_decl       { [$1] }
  | source_decl       { $1 }
  | sig_or_ass        { $1 }

sig_or_ass :: { [Loc CstExpr] }
  : evar_or_op lower_names '::' sig_type
      { [at $1 (CSigE (toEVar $1) $2 $4)] }
  | evar_or_op lower_names '=' expr opt_where_decls
      { [at $1 (CAssE (toEVar $1) $2 $4 $5)] }

--------------------------------------------------------------------
-- Module names
--------------------------------------------------------------------

module_name :: { Text }
  : module_parts             { T.intercalate "." $1 }

module_parts :: { [Text] }
  : module_comp                        { [$1] }
  | module_parts '.' module_comp       { $1 ++ [$3] }
  | module_parts GDOT module_comp      { $1 ++ [$3] }

module_comp :: { Text }
  : LOWER                              { getName $1 }
  | module_comp '-' LOWER              { $1 <> "-" <> getName $3 }

--------------------------------------------------------------------
-- Exports
--------------------------------------------------------------------

exports :: { CstExport }
  : '*'                     { CstExportAll }
  | export_list             { CstExportMany $1 }

export_list :: { [Located] }
  : export_item                      { [$1] }
  | export_list ',' export_item      { $1 ++ [$3] }

export_item :: { Located }
  : symbol              { $1 }

symbol :: { Located }
  : LOWER               { $1 }
  | '(' operator_name ')' { $2 }
  | '(' '-' ')'          { $2 }
  | '(' '.' ')'          { $2 }
  | UPPER               { $1 }

--------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------

import_decl :: { Loc CstExpr }
  : 'import' module_name opt_import_list
      { at $1 (CImpE (Import (MV $2) $3 [] Nothing)) }

opt_import_list :: { Maybe [AliasedSymbol] }
  : {- empty -}                            { Nothing }
  | '(' import_items ')'                   { Just $2 }

import_items :: { [AliasedSymbol] }
  : import_item                            { [$1] }
  | import_items ',' import_item           { $1 ++ [$3] }

import_item :: { AliasedSymbol }
  : LOWER                              { AliasedTerm (EV (getName $1)) (EV (getName $1)) }
  | LOWER 'as' LOWER                   { AliasedTerm (EV (getName $1)) (EV (getName $3)) }
  | '(' operator_name ')' 'as' LOWER   { AliasedTerm (EV (getOp $2)) (EV (getName $5)) }
  | '(' operator_name ')'              { AliasedTerm (EV (getOp $2)) (EV (getOp $2)) }
  | '(' '-' ')' 'as' LOWER            { AliasedTerm (EV "-") (EV (getName $5)) }
  | '(' '-' ')'                        { AliasedTerm (EV "-") (EV "-") }
  | '(' '.' ')' 'as' LOWER            { AliasedTerm (EV ".") (EV (getName $5)) }
  | '(' '.' ')'                        { AliasedTerm (EV ".") (EV ".") }
  | UPPER                              { AliasedType (TV (getName $1)) (TV (getName $1)) }
  | UPPER 'as' UPPER                   { AliasedType (TV (getName $1)) (TV (getName $3)) }

--------------------------------------------------------------------
-- Type definitions
--------------------------------------------------------------------

typedef_decl :: { Loc CstExpr }
  : 'type' UPPER '=>' typedef_term '=' concrete_rhs
      { at $1 (CTypE (CstTypeAlias (Just $2) $4 $6)) }
  | 'type' LOWER '=>' typedef_term '=' concrete_rhs
      { at $1 (CTypE (CstTypeAlias (Just $2) $4 $6)) }
  | 'type' UPPER typedef_params '=' type
      { at $1 (CTypE (CstTypeAlias Nothing (TV (getName $2), $3) ($5, False))) }
  | 'type' UPPER typedef_params
      { at $1 (CTypE (CstTypeAliasForward (TV (getName $2), $3))) }
  | 'type' '(' UPPER typedef_params ')' '=' type
      { at $1 (CTypE (CstTypeAlias Nothing (TV (getName $3), $4) ($7, False))) }
  | 'type' '(' UPPER typedef_params ')'
      { at $1 (CTypE (CstTypeAliasForward (TV (getName $3), $4))) }
  | nam_type typedef_term 'where' VLBRACE nam_entry_list_loc VRBRACE
      { at (fst $1) (CTypE (CstNamTypeWhere (snd $1) $2 $5)) }
  | nam_type typedef_term '=' nam_constructor opt_nam_entries
      { at (fst $1) (CTypE (CstNamTypeLegacy Nothing (snd $1) $2 $4 $5)) }
  | nam_type UPPER '=>' typedef_term '=' nam_constructor opt_nam_entries
      { at (fst $1) (CTypE (CstNamTypeLegacy (Just $2) (snd $1) $4 $6 $7)) }
  | nam_type LOWER '=>' typedef_term '=' nam_constructor opt_nam_entries
      { at (fst $1) (CTypE (CstNamTypeLegacy (Just $2) (snd $1) $4 $6 $7)) }

nam_type :: { (Located, NamType) }
  : 'record'   { ($1, NamRecord) }
  | 'object'   { ($1, NamObject) }
  | 'table'    { ($1, NamTable) }

nam_constructor :: { (Text, Bool) }
  : STRING                    { (getString $1, True) }
  | UPPER                     { (getName $1, False) }
  | LOWER                     { (getName $1, False) }

opt_nam_entries :: { [(Key, TypeU)] }
  : {- empty -}              { [] }
  | '{' nam_entries '}'       { $2 }

lang_token :: { Located }
  : UPPER                    { $1 }
  | LOWER                    { $1 }

typedef_term :: { (TVar, [Either TVar TypeU]) }
  : UPPER typedef_params              { (TV (getName $1), $2) }
  | '(' UPPER typedef_params ')'     { (TV (getName $2), $3) }

typedef_params :: { [Either TVar TypeU] }
  : {- empty -}                        { [] }
  | typedef_params LOWER               { $1 ++ [Left (TV (getName $2))] }
  | typedef_params '(' type ')'        { $1 ++ [Right $3] }

nam_entry :: { (Key, TypeU) }
  : LOWER '::' type          { (Key (getName $1), $3) }

nam_entry_loc :: { (Located, Key, TypeU) }
  : LOWER '::' type          { ($1, Key (getName $1), $3) }

nam_entry_list_loc :: { [(Located, Key, TypeU)] }
  : nam_entry_loc                              { [$1] }
  | nam_entry_list_loc VSEMI nam_entry_loc     { $1 ++ [$3] }

nam_entries :: { [(Key, TypeU)] }
  : nam_entry                          { [$1] }
  | nam_entries ',' nam_entry          { $1 ++ [$3] }

concrete_rhs :: { (TypeU, Bool) }
  : STRING concrete_rhs_args    { (case $2 of { [] -> VarU (TV (getString $1)); ts -> AppU (VarU (TV (getString $1))) ts }, True) }
  | non_string_type             { ($1, False) }

concrete_rhs_args :: { [TypeU] }
  : {- empty -}                       { [] }
  | concrete_rhs_args atom_type       { $1 ++ [$2] }

non_string_type :: { TypeU }
  : non_string_non_fun '->' type  { case $3 of { FunU args ret -> FunU ($1 : args) ret; t -> FunU [$1] t } }
  | non_string_non_fun            { $1 }

non_string_non_fun :: { TypeU }
  : '<' LOWER '>'            { ExistU (TV (getName $2)) ([], Open) ([], Open) }
  | non_string_app            { $1 }

non_string_app :: { TypeU }
  : non_string_app atom_type  { applyType $1 $2 }
  | non_string_atom           { $1 }

non_string_atom :: { TypeU }
  : '(' ')'                  { BT.unitU }
  | '(' type ')'             { $2 }
  | '(' type ',' type_list1 ')' { BT.tupleU ($2 : $4) }
  | '[' type ']'              { BT.listU $2 }
  | '{' type '}'              { ThunkU $2 }
  | UPPER                     { VarU (TV (getName $1)) }
  | LOWER ':' non_fun_type   { $3 }
  | LOWER                     { VarU (TV (getName $1)) }

--------------------------------------------------------------------
-- Typeclasses
--------------------------------------------------------------------

typeclass_decl :: { Loc CstExpr }
  : 'class' class_head 'where' VLBRACE sig_list VRBRACE
      { at $1 (CClsE $2 $5) }
  | 'class' class_head
      { at $1 (CClsE $2 []) }

class_head :: { CstClassHead }
  : app_type '=>' app_type
      { CCHConstrained $1 $3 }
  | '(' class_constraints ')' '=>' app_type
      { CCHMultiConstrained $2 $5 }
  | app_type
      { CCHSimple $1 }

class_constraints :: { [Constraint] }
  : single_constraint                            { [$1] }
  | class_constraints ',' single_constraint      { $1 ++ [$3] }

sig_list :: { [CstSigItem] }
  : signature                     { [$1] }
  | sig_list VSEMI signature      { $1 ++ [$3] }

signature :: { CstSigItem }
  : evar_or_op lower_names '::' sig_type
      { CstSigItem (toEVar $1) $2 $4 }

--------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------

instance_decl :: { Loc CstExpr }
  : 'instance' UPPER types1 'where' VLBRACE instance_items VRBRACE
      { at $1 (CIstE (ClassName (getName $2)) $3 (concat $6)) }
  | 'instance' UPPER types1
      { at $1 (CIstE (ClassName (getName $2)) $3 []) }

instance_items :: { [[Loc CstExpr]] }
  : instance_item                        { [$1] }
  | instance_items VSEMI instance_item   { $1 ++ [$3] }

instance_item :: { [Loc CstExpr] }
  : source_decl             { $1 }
  | sig_or_ass              { $1 }

--------------------------------------------------------------------
-- Fixity declarations
--------------------------------------------------------------------

fixity_decl :: { Loc CstExpr }
  : 'infixl' INTEGER operator_names
      { at $1 (CFixE InfixL (fromInteger (getInt $2)) $3) }
  | 'infixr' INTEGER operator_names
      { at $1 (CFixE InfixR (fromInteger (getInt $2)) $3) }
  | 'infix' INTEGER operator_names
      { at $1 (CFixE InfixN (fromInteger (getInt $2)) $3) }

operator_names :: { [EVar] }
  : operator_ref                         { [$1] }
  | operator_names ',' operator_ref      { $1 ++ [$3] }

operator_ref :: { EVar }
  : '(' operator_name ')'     { EV (getOp $2) }
  | '(' '-' ')'               { EV "-" }
  | '(' '.' ')'               { EV "." }
  | operator_name              { EV (getOp $1) }
  | '.'                        { EV "." }
  | '-'                        { EV "-" }
  | LOWER                      { EV (getName $1) }

--------------------------------------------------------------------
-- Source declarations
--------------------------------------------------------------------

source_decl :: { [Loc CstExpr] }
  : 'source' lang_token opt_from '(' source_items ')'
      { [at $1 (CSrcOldE $2 $3 $5)] }
  | 'source' lang_token opt_from 'where' VLBRACE source_new_items VRBRACE
      { [at $1 (CSrcNewE $2 $3 $6)] }

opt_from :: { Maybe Text }
  : {- empty -}                    { Nothing }
  | 'from' STRING                  { Just (getString $2) }

source_items :: { [(Text, Maybe Text)] }
  : source_item                          { [$1] }
  | source_items ',' source_item         { $1 ++ [$3] }

source_item :: { (Text, Maybe Text) }
  : STRING                              { (getString $1, Nothing) }
  | STRING 'as' LOWER                   { (getString $1, Just (getName $3)) }
  | STRING 'as' UPPER                   { (getString $1, Just (getName $3)) }
  | STRING 'as' '(' operator_name ')'   { (getString $1, Just (getOp $4)) }
  | STRING 'as' '(' '-' ')'            { (getString $1, Just "-") }
  | STRING 'as' '(' '.' ')'            { (getString $1, Just ".") }

source_new_items :: { [Located] }
  : source_new_item                          { [$1] }
  | source_new_items VSEMI source_new_item   { $1 ++ [$3] }

source_new_item :: { Located }
  : LOWER                              { $1 }

--------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------

expr :: { Loc CstExpr }
  : let_expr                { $1 }
  | lambda_expr             { $1 }
  | infix_expr              { $1 }
  | infix_expr '::' type    { at $2 (CAnnE $1 $3) }

let_expr :: { Loc CstExpr }
  : let_bindings 'in' expr
      { at $2 (CLetE $1 $3) }

let_bindings :: { [(EVar, Loc CstExpr)] }
  : let_binding                        { [$1] }
  | let_bindings let_binding           { $1 ++ [$2] }

let_binding :: { (EVar, Loc CstExpr) }
  : 'let' LOWER '=' expr              { (EV (getName $2), $4) }
  | 'let' '_' '=' expr                { (EV "_", $4) }

lambda_expr :: { Loc CstExpr }
  : '\\' lower_names1 '->' expr
      { at $1 (CLamE (map EV $2) $4) }

infix_expr :: { Loc CstExpr }
  : operand                  { $1 }
  | operand operator_name expr
      { at $2 (CBopE $1 $2 $3) }
  | operand '-' expr
      { at $2 (CBopE $1 $2 $3) }
  | operand '.' expr
      { at $2 (CBopE $1 $2 $3) }

operand :: { Loc CstExpr }
  : app_expr                 { $1 }
  | '-' INTEGER              { at $1 (CIntE (negate (getInt $2))) }
  | '-' FLOAT                { at $1 (CRealE (DS.fromFloatDigits (negate (getFloat $2)))) }

app_expr :: { Loc CstExpr }
  : atom_expr                      { $1 }
  | atom_expr atom_exprs1          { Loc ($1 <-> last $2) (CAppE $1 $2) }

atom_exprs1 :: { [Loc CstExpr] }
  : atom_expr                      { [$1] }
  | atom_exprs1 atom_expr          { $1 ++ [$2] }

atom_expr :: { Loc CstExpr }
  : force_expr                { $1 }
  | paren_expr                { $1 }
  | getter_expr               { $1 }
  | string_expr               { $1 }
  | bool_expr                 { $1 }
  | num_expr                  { $1 }
  | list_expr                 { $1 }
  | suspend_expr              { $1 }
  | record_expr               { $1 }
  | var_expr                  { $1 }
  | hole_expr                 { $1 }
  | do_expr                   { $1 }

force_expr :: { Loc CstExpr }
  : '!' atom_expr             { at $1 (CForceE $2) }

paren_expr :: { Loc CstExpr }
  : '(' ')'                   { at $1 CUniE }
  | '(' operator_name ')'     { at $1 (CVarE (EV (getOp $2))) }
  | '(' '-' ')'               { at $1 (CVarE (EV "-")) }
  | '(' '.' ')'               { at $1 (CVarE (EV ".")) }
  | '(' expr ')'              { $2 }
  | '(' expr ',' expr_list1 ')' { Loc ($1 <-> $5) (CTupE ($2 : $4)) }

expr_list1 :: { [Loc CstExpr] }
  : expr                       { [$1] }
  | expr_list1 ',' expr        { $1 ++ [$3] }

suspend_expr :: { Loc CstExpr }
  : '{' expr '}'              { Loc ($1 <-> $3) (CSuspendE $2) }

record_expr :: { Loc CstExpr }
  : '{' record_entries '}'    { Loc ($1 <-> $3) (CNamE $2) }

record_entries :: { [(Key, Loc CstExpr)] }
  : record_entry                         { [$1] }
  | record_entries ',' record_entry      { $1 ++ [$3] }

record_entry :: { (Key, Loc CstExpr) }
  : LOWER '=' expr                       { (Key (getName $1), $3) }

list_expr :: { Loc CstExpr }
  : '[' ']'                   { Loc ($1 <-> $2) (CLstE []) }
  | '[' expr_list1 ']'        { Loc ($1 <-> $3) (CLstE $2) }

do_expr :: { Loc CstExpr }
  : 'do' VLBRACE do_stmts VRBRACE     { Loc ($1 <-> $4) (CDoE $3) }

do_stmts :: { [CstDoStmt] }
  : do_stmt                   { [$1] }
  | do_stmts VSEMI do_stmt    { $1 ++ [$3] }

do_stmt :: { CstDoStmt }
  : LOWER '<-' expr            { CstDoBind (EV (getName $1)) $3 }
  | expr                       { CstDoBare $1 }

getter_expr :: { Loc CstExpr }
  : GDOT accessor_body    { at $1 (CAccessorE $2) }

accessor_body :: { CstAccessorBody }
  : LOWER accessor_tail           { CABKey (getName $1) $2 }
  | INTEGER accessor_tail         { CABIdx (fromInteger (getInt $1)) $2 }
  | '(' grouped_accessors ')'    { CABGroup $2 }

accessor_tail :: { CstAccessorTail }
  : {- empty -}                   { CATEnd }
  | '=' expr                      { CATSet $2 }
  | GDOT accessor_body            { CATChain $2 }

grouped_accessors :: { [CstAccessorBody] }
  : grouped_accessor                          { [$1] }
  | grouped_accessors ',' grouped_accessor   { $1 ++ [$3] }

grouped_accessor :: { CstAccessorBody }
  : GDOT accessor_body   { $2 }

var_expr :: { Loc CstExpr }
  : LOWER                     { at $1 (CVarE (EV (getName $1))) }

hole_expr :: { Loc CstExpr }
  : '_'                        { at $1 CHolE }

bool_expr :: { Loc CstExpr }
  : 'True'                     { at $1 (CLogE True) }
  | 'False'                    { at $1 (CLogE False) }

num_expr :: { Loc CstExpr }
  : INTEGER                    { at $1 (CIntE (getInt $1)) }
  | FLOAT                      { at $1 (CRealE (DS.fromFloatDigits (getFloat $1))) }

string_expr :: { Loc CstExpr }
  : STRING                     { at $1 (CStrE (getString $1)) }
  | interp_string              { $1 }

interp_string :: { Loc CstExpr }
  : STRSTART interp_body STREND
      { Loc ($1 <-> $3) (CInterpE (getString $1) (fst $2) (snd $2) (getString $3)) }

interp_body :: { ([Loc CstExpr], [Text]) }
  : INTERPOPEN expr INTERPCLOSE
      { ([$2], []) }
  | interp_body STRMID INTERPOPEN expr INTERPCLOSE
      { let (es, ms) = $1 in (es ++ [$4], ms ++ [getString $2]) }

--------------------------------------------------------------------
-- Types
--------------------------------------------------------------------

type :: { TypeU }
  : fun_type                 { $1 }
  | non_fun_type             { $1 }

fun_type :: { TypeU }
  : non_fun_type '->' type   { case $3 of { FunU args ret -> FunU ($1 : args) ret; t -> FunU [$1] t } }

non_fun_type :: { TypeU }
  : '<' LOWER '>'            { ExistU (TV (getName $2)) ([], Open) ([], Open) }
  | app_type                  { $1 }

app_type :: { TypeU }
  : app_type atom_type        { applyType $1 $2 }
  | atom_type                 { $1 }

atom_type :: { TypeU }
  : '(' ')'                  { BT.unitU }
  | '(' type ')'             { $2 }
  | '(' type ',' type_list1 ')' { BT.tupleU ($2 : $4) }
  | '[' type ']'              { BT.listU $2 }
  | '{' type '}'              { ThunkU $2 }
  | UPPER                     { VarU (TV (getName $1)) }
  | LOWER ':' non_fun_type   { $3 }
  | LOWER                     { VarU (TV (getName $1)) }
  | STRING                    { VarU (TV (getString $1)) }

type_list1 :: { [TypeU] }
  : type                      { [$1] }
  | type_list1 ',' type       { $1 ++ [$3] }

types1 :: { [TypeU] }
  : atom_type                  { [$1] }
  | types1 atom_type           { $1 ++ [$2] }

--------------------------------------------------------------------
-- Constraints and signature types
--------------------------------------------------------------------

sig_type :: { CstSigType }
  : sig_fun_args '=>' sig_fun_args
      { CstSigType (Just $1) $3 }
  | sig_fun_args
      { CstSigType Nothing $1 }

sig_fun_args :: { [(Pos, TypeU)] }
  : pos_non_fun_type '->' sig_fun_args  { $1 : $3 }
  | pos_non_fun_type                     { [$1] }

pos_non_fun_type :: { (Pos, TypeU) }
  : '<' LOWER '>'     { (locPos $1, ExistU (TV (getName $2)) ([], Open) ([], Open)) }
  | pos_app_type       { $1 }

pos_app_type :: { (Pos, TypeU) }
  : pos_app_type atom_type  { (fst $1, applyType (snd $1) $2) }
  | pos_atom_type            { $1 }

pos_atom_type :: { (Pos, TypeU) }
  : '(' ')'                     { (locPos $1, BT.unitU) }
  | '(' type ')'                { (locPos $1, $2) }
  | '(' type ',' type_list1 ')' { (locPos $1, BT.tupleU ($2 : $4)) }
  | '[' type ']'                { (locPos $1, BT.listU $2) }
  | '{' type '}'                { (locPos $1, ThunkU $2) }
  | UPPER                       { (locPos $1, VarU (TV (getName $1))) }
  | LOWER ':' non_fun_type      { (locPos $1, $3) }
  | LOWER                       { (locPos $1, VarU (TV (getName $1))) }
  | STRING                      { (locPos $1, VarU (TV (getString $1))) }

single_constraint :: { Constraint }
  : UPPER types1                         { Constraint (ClassName (getName $1)) $2 }

--------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------

operator_name :: { Located }
  : OPERATOR                  { $1 }
  | '*'                       { $1 }
  | '<'                       { $1 }
  | '>'                       { $1 }

evar_or_op :: { Located }
  : LOWER                     { $1 }
  | '(' operator_name ')'     { $2 }
  | '(' '-' ')'               { $2 }
  | '(' '.' ')'               { $2 }

opt_where_decls :: { [Loc CstExpr] }
  : {- empty -}                               { [] }
  | 'where' VLBRACE where_items VRBRACE       { $3 }

where_items :: { [Loc CstExpr] }
  : where_item                      { $1 }
  | where_items VSEMI where_item    { $1 ++ $3 }

where_item :: { [Loc CstExpr] }
  : sig_or_ass                { $1 }

lower_names :: { [Text] }
  : {- empty -}               { [] }
  | lower_names LOWER         { $1 ++ [getName $2] }

lower_names1 :: { [Text] }
  : LOWER                     { [getName $1] }
  | lower_names1 LOWER        { $1 ++ [getName $2] }

{

--------------------------------------------------------------------
-- Parser monad
--------------------------------------------------------------------

data PState = PState
  { psExpIndex    :: !Int
  , psSourceMap   :: !(Map.Map Int SrcLoc)
  , psModulePath  :: !(Maybe Path)
  , psModuleConfig :: !ModuleConfig
  , psDocMap      :: !(Map.Map Pos [Text])
  , psSourceLines :: ![Text]
  , psLangMap :: !(Map.Map T.Text Lang) -- alias -> Lang for all known languages
  }
  deriving (Show)

emptyPState :: PState
emptyPState = PState 1 Map.empty Nothing defaultValue Map.empty [] Map.empty

type P a = State.StateT PState (Either ParseError) a

--------------------------------------------------------------------
-- Token extraction helpers
--------------------------------------------------------------------

getName :: Located -> Text
getName (Located _ (TokLowerName n) _) = n
getName (Located _ (TokUpperName n) _) = n
getName (Located _ _ t) = t

getInt :: Located -> Integer
getInt (Located _ (TokInteger n) _) = n
getInt _ = 0

getFloat :: Located -> Double
getFloat (Located _ (TokFloat d) _) = d
getFloat _ = 0

getString :: Located -> Text
getString (Located _ (TokString s) _) = s
getString (Located _ (TokStringStart s) _) = s
getString (Located _ (TokStringMid s) _) = s
getString (Located _ (TokStringEnd s) _) = s
getString (Located _ _ t) = t

getOp :: Located -> Text
getOp (Located _ (TokOperator t) _) = t
getOp (Located _ TokMinus _) = "-"
getOp (Located _ TokStar _) = "*"
getOp (Located _ TokDot _) = "."
getOp (Located _ TokLAngle _) = "<"
getOp (Located _ TokRAngle _) = ">"
getOp (Located _ _ t) = t

toEVar :: Located -> EVar
toEVar (Located _ (TokLowerName n) _) = EV n
toEVar (Located _ (TokOperator n) _) = EV n
toEVar (Located _ TokMinus _) = EV "-"
toEVar (Located _ TokStar _) = EV "*"
toEVar (Located _ TokDot _) = EV "."
toEVar (Located _ TokLAngle _) = EV "<"
toEVar (Located _ TokRAngle _) = EV ">"
toEVar _ = EV "?"

--------------------------------------------------------------------
-- Type helper
--------------------------------------------------------------------

applyType :: TypeU -> TypeU -> TypeU
applyType (AppU f args) x = AppU f (args ++ [x])
applyType f x = AppU f [x]

--------------------------------------------------------------------
-- Error handling
--------------------------------------------------------------------

parseError :: ([Located], [String]) -> P a
parseError ([], expected) = do
  srcLines <- State.gets psSourceLines
  State.lift (Left (ParseError (Pos 0 0 "") "unexpected end of input" expected srcLines))
parseError (Located pos tok _ : _, expected) = do
  srcLines <- State.gets psSourceLines
  State.lift (Left (ParseError pos ("unexpected " ++ showToken tok) expected srcLines))

--------------------------------------------------------------------
-- Desugar bridge
--------------------------------------------------------------------

toDState :: PState -> DState
toDState ps = DState
  { dsExpIndex = psExpIndex ps
  , dsSourceMap = psSourceMap ps
  , dsDocMap = psDocMap ps
  , dsModulePath = psModulePath ps
  , dsModuleConfig = psModuleConfig ps
  , dsSourceLines = psSourceLines ps
  , dsLangMap = psLangMap ps
  }

fromDState :: PState -> DState -> PState
fromDState ps ds = ps
  { psExpIndex = dsExpIndex ds
  , psSourceMap = dsSourceMap ds
  }

-- | Run parse + desugar
parseAndDesugar :: PState -> [Located] -> Either ParseError ([ExprI], PState)
parseAndDesugar pstate tokens =
  case State.runStateT (parseProgram tokens) pstate of
    Left err -> Left err
    Right ((cstNodes, isImplicitMain), _parseState) ->
      let dstate = toDState pstate
      in case State.runStateT (desugarProgram isImplicitMain cstNodes) dstate of
        Left err -> Left err
        Right (exprIs, finalDState) ->
          Right (exprIs, fromDState pstate finalDState)

-- | Parse and desugar a single expression
parseAndDesugarExpr :: PState -> [Located] -> Either ParseError (ExprI, PState)
parseAndDesugarExpr pstate tokens =
  case State.runStateT (parseExprOnly tokens) pstate of
    Left err -> Left err
    Right (cstExpr, _parseState) ->
      let dstate = toDState pstate
      in case State.runStateT (desugarExpr cstExpr) dstate of
        Left err -> Left err
        Right (exprI, finalDState) ->
          Right (exprI, fromDState pstate finalDState)

--------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------

readProgram ::
  Maybe MVar ->
  Maybe Path ->
  Text ->
  PState ->
  DAG MVar Import ExprI ->
  Either String (DAG MVar Import ExprI, PState)
readProgram _moduleName modulePath sourceCode pstate dag = do
  let filename = maybe "<expr>" id modulePath
  (tokens, docMap) <- case lexMorloc filename sourceCode of
    Left err -> Left (showLexError err)
    Right r -> Right r
  let srcLines = T.lines sourceCode
      pstate' = pstate { psModulePath = modulePath, psDocMap = docMap, psSourceLines = srcLines }
  -- Strategy 1: parse as-is (code with module declarations)
  case parseAndDesugar pstate' tokens of
    Right (result, finalState) ->
      let dag' = foldl addModule dag result
      in return (dag', finalState)
    Left err ->
      -- Strategy 2: wrap in module, patch trailing expr as __expr__ assignment.
      let wrappedCode = "module main (*)\n" <> sourceCode
      in case lexMorloc filename wrappedCode of
        Right (wrappedTokens, wrappedDocMap) ->
          let pstate'' = pstate' { psDocMap = wrappedDocMap, psSourceLines = T.lines wrappedCode }
          in case parseAndDesugar pstate'' wrappedTokens of
            Right (result, finalState) ->
              let dag' = foldl addModule dag result
              in return (dag', finalState)
            Left _ ->
              case patchForTrailingExpr wrappedTokens of
                Just patchedTokens ->
                  case parseAndDesugar pstate'' patchedTokens of
                    Right (result, finalState) ->
                      let dag' = foldl addModule dag result
                      in return (dag', finalState)
                    Left _ -> tryExprFallback tokens pstate' dag filename err
                Nothing -> tryExprFallback tokens pstate' dag filename err
        Left _ -> tryExprFallback tokens pstate' dag filename err
  where
    tryExprFallback tokens' ps dag' filename' origErr =
      let exprTokens = stripLayoutTokens tokens'
      in case parseAndDesugarExpr ps exprTokens of
        Right (exprI, exprState) -> do
          let s = exprState
              i1 = psExpIndex s
              assI = ExprI i1 (AssE (EV "__expr__") exprI [])
              s1 = s { psExpIndex = i1 + 1 }
              i2 = psExpIndex s1
              expI = ExprI i2 (ExpE ExportAll)
              s2 = s1 { psExpIndex = i2 + 1 }
              i3 = psExpIndex s2
              modI = ExprI i3 (ModE (MV "main") [expI, assI])
              finalState = s2 { psExpIndex = i3 + 1 }
              dag'' = Map.insert (MV "main") (modI, []) dag'
          return (dag'', finalState)
        Left _ ->
          Left (showParseError filename' origErr)

    addModule d e@(ExprI _ (ModE n es)) =
      let imports = [(importModuleName i, i) | (ExprI _ (ImpE i)) <- es]
      in Map.insert n (e, imports) d
    addModule _ _ = error "expected a module"

patchForTrailingExpr :: [Located] -> Maybe [Located]
patchForTrailingExpr tokens = do
  let tokens' = patchExport tokens
  patchLastStmt tokens'

patchExport :: [Located] -> [Located]
patchExport [] = []
patchExport (t@(Located _ TokLParen _) : Located p TokStar _ : rest) =
  t : Located p (TokLowerName "__expr__") "__expr__" : rest
patchExport (t : rest) = t : patchExport rest

patchLastStmt :: [Located] -> Maybe [Located]
patchLastStmt tokens =
  case findLastTopVSemi tokens 0 0 Nothing of
    Just idx ->
      let (before, after) = splitAt (idx + 1) tokens
          dummyPos = Pos 0 0 "<expr>"
          exprTok = Located dummyPos (TokLowerName "__expr__") "__expr__"
          eqTok = Located dummyPos TokEquals "="
      in Just (before ++ [exprTok, eqTok] ++ after)
    Nothing -> Nothing
  where
    findLastTopVSemi :: [Located] -> Int -> Int -> Maybe Int -> Maybe Int
    findLastTopVSemi [] _ _ lastIdx = lastIdx
    findLastTopVSemi (Located _ TokVLBrace _ : rest) depth pos lastIdx =
      findLastTopVSemi rest (depth + 1) (pos + 1) lastIdx
    findLastTopVSemi (Located _ TokVRBrace _ : rest) depth pos lastIdx =
      findLastTopVSemi rest (max 0 (depth - 1)) (pos + 1) lastIdx
    findLastTopVSemi (Located _ TokVSemi _ : rest) depth pos _
      | depth == 1 = findLastTopVSemi rest depth (pos + 1) (Just pos)
    findLastTopVSemi (_ : rest) depth pos lastIdx =
      findLastTopVSemi rest depth (pos + 1) lastIdx

stripLayoutTokens :: [Located] -> [Located]
stripLayoutTokens = filter (not . isLayoutToken)
  where
    isLayoutToken (Located _ TokVLBrace _) = True
    isLayoutToken (Located _ TokVRBrace _) = True
    isLayoutToken (Located _ TokVSemi _) = True
    isLayoutToken _ = False

readType :: Text -> Either String TypeU
readType typeStr = do
  let initState = emptyPState
  (tokens, _) <- case lexMorloc "<type>" typeStr of
    Left err -> Left (showLexError err)
    Right r -> Right r
  (result, _) <- case State.runStateT (parseTypeOnly tokens) initState of
    Left err -> Left (showParseError "<type>" err)
    Right r -> Right r
  return result
}
