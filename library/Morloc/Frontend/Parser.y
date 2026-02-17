{
{-# LANGUAGE OverloadedStrings #-}

module Morloc.Frontend.Parser
  ( readProgram
  , readType
  , PState (..)
  , emptyPState
  ) where

import qualified Control.Monad.State.Strict as State
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Scientific as DS
import Data.Char (isLower)
import Data.List (intercalate, nub)
import Data.Maybe (mapMaybe)
import Morloc.Frontend.Token
import Morloc.Frontend.Lexer (lexMorloc, showLexError)
import Morloc.Namespace.Prim
import Morloc.Namespace.Type
import Morloc.Namespace.Expr
import qualified Morloc.BaseTypes as BT
import qualified Morloc.Language as ML
import System.FilePath (takeDirectory, combine)
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

program :: { [ExprI] }
  : modules EOF             { $1 }
  | top_body EOF            {% mkImplicitMain $1 }

-- Standalone entry points with explicit EOF
type_eof :: { TypeU }
  : type EOF                { $1 }

expr_eof :: { ExprI }
  : expr EOF                { $1 }

modules :: { [ExprI] }
  : module                   { [$1] }
  | modules module           { $1 ++ [$2] }

module :: { ExprI }
  : 'module' module_name '(' exports ')' top_body
      {% do { i <- freshId $1
            ; expI <- freshExprI $1 (ExpE $4)
            ; return (ExprI i (ModE (MV $2) (expI : $6))) } }

top_body :: { [ExprI] }
  : VLBRACE top_decls VRBRACE   { $2 }
  | VLBRACE VRBRACE              { [] }

top_decls :: { [ExprI] }
  : top_decl                     { $1 }
  | top_decls VSEMI top_decl     { $1 ++ $3 }

top_decl :: { [ExprI] }
  : import_decl       { [$1] }
  | typedef_decl      { [$1] }
  | typeclass_decl    { [$1] }
  | instance_decl     { [$1] }
  | fixity_decl       { [$1] }
  | source_decl       { $1 }
  | sig_or_ass        { $1 }

-- Disambiguate signature vs assignment:
-- Both start with LOWER (or paren-op). Signature has :: after optional params.
-- Assignment has = after optional params.
sig_or_ass :: { [ExprI] }
  : evar_or_op lower_names '::' sig_type
      {% do { docs <- lookupDocs $1
            ; let cmdDoc = processArgDocLines docs
            ; let (cs, argDocs, t) = $4
            ; let t' = forallWrap (map TV $2) t
            ; let doc = ArgDocSig cmdDoc (init argDocs) (last argDocs)
            ; let et = EType t' (Set.fromList cs) doc
            ; e <- freshExprI $1 (SigE (Signature (toEVar $1) Nothing et))
            ; return [e] } }
  | evar_or_op lower_names '=' expr opt_where_decls
      {% do { e <- case $2 of
                [] -> freshExprI $1 (AssE (toEVar $1) $4 $5)
                vs -> do { lam <- freshExprI $3 (LamE (map EV vs) $4)
                         ; freshExprI $1 (AssE (toEVar $1) lam $5) }
            ; return [e] } }

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

exports :: { Export }
  : '*'                     { ExportAll }
  | export_list             { ExportMany (Set.fromList $1) }

export_list :: { [(Int, Symbol)] }
  : export_item                      { [$1] }
  | export_list ',' export_item      { $1 ++ [$3] }

export_item :: { (Int, Symbol) }
  : symbol              {% do { i <- freshId $1; return (i, symVal $1) } }

symbol :: { Located }
  : LOWER               { $1 }
  | '(' operator_name ')' { $2 }
  | '(' '-' ')'          { $2 }
  | '(' '.' ')'          { $2 }
  | UPPER               { $1 }

--------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------

import_decl :: { ExprI }
  : 'import' module_name opt_import_list
      {% freshExprI $1 (ImpE (Import (MV $2) $3 [] Nothing)) }

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

typedef_decl :: { ExprI }
  -- After 'type', the first UPPER could be a language name (followed by =>)
  -- or the type name itself. We consume UPPER first and let the next token
  -- (=> vs = vs LOWER vs end) disambiguate -- no shift/reduce conflict.
  : 'type' UPPER '=>' typedef_term '=' concrete_rhs
      {% do { lang <- parseLang (getName $2)
            ; let (t, isTerminal) = $6
            ; mkTypedef $1 (Just (lang, isTerminal)) $4 t } }
  | 'type' LOWER '=>' typedef_term '=' concrete_rhs
      {% do { lang <- parseLang (getName $2)
            ; let (t, isTerminal) = $6
            ; mkTypedef $1 (Just (lang, isTerminal)) $4 t } }
  | 'type' UPPER typedef_params '=' type
      {% mkTypedef $1 Nothing (TV (getName $2), $3) $5 }
  | 'type' UPPER typedef_params
      {% mkTypedefAlias $1 (TV (getName $2), $3) }
  | 'type' '(' UPPER typedef_params ')' '=' type
      {% mkTypedef $1 Nothing (TV (getName $3), $4) $7 }
  | 'type' '(' UPPER typedef_params ')'
      {% mkTypedefAlias $1 (TV (getName $3), $4) }
  | nam_type typedef_term 'where' VLBRACE nam_entry_list_loc VRBRACE
      {% mkNamTypeWhereWithDocs (fst $1) $3 (snd $1) $2 $5 }
  | nam_type typedef_term '=' nam_constructor opt_nam_entries
      {% mkNamTypeLegacy $3 (snd $1) $2 (Just (fst $4)) Nothing $5 }
  | nam_type UPPER '=>' typedef_term '=' nam_constructor opt_nam_entries
      {% do { lang <- parseLang (getName $2)
            ; mkNamTypeLegacy $5 (snd $1) $4 (Just (fst $6)) (Just (lang, snd $6)) $7 } }
  | nam_type LOWER '=>' typedef_term '=' nam_constructor opt_nam_entries
      {% do { lang <- parseLang (getName $2)
            ; mkNamTypeLegacy $5 (snd $1) $4 (Just (fst $6)) (Just (lang, snd $6)) $7 } }

nam_type :: { (Located, NamType) }
  : 'record'   { ($1, NamRecord) }
  | 'object'   { ($1, NamObject) }
  | 'table'    { ($1, NamTable) }

-- Constructor name and whether it's a concrete (terminal) type
nam_constructor :: { (Text, Bool) }
  : STRING                    { (getString $1, True) }
  | UPPER                     { (getName $1, False) }
  | LOWER                     { (getName $1, False) }

opt_nam_entries :: { [(Key, TypeU)] }
  : {- empty -}              { [] }
  | '{' nam_entries '}'       { $2 }

lang_name :: { Lang }
  : UPPER                    {% parseLang (getName $1) }
  | LOWER                    {% parseLang (getName $1) }

typedef_term :: { (TVar, [Either TVar TypeU]) }
  : UPPER typedef_params              { (TV (getName $1), $2) }
  | '(' UPPER typedef_params ')'     { (TV (getName $2), $3) }

typedef_params :: { [Either TVar TypeU] }
  : {- empty -}                        { [] }
  | typedef_params LOWER               { $1 ++ [Left (TV (getName $2))] }
  | typedef_params '(' type ')'        { $1 ++ [Right $3] }

nam_entry :: { (Key, TypeU) }
  : LOWER '::' type          { (Key (getName $1), $3) }

-- | Record field entry with docstring position tracking
nam_entry_loc :: { (Located, Key, TypeU) }
  : LOWER '::' type          { ($1, Key (getName $1), $3) }

-- | Record field entries with docstring position tracking
nam_entry_list_loc :: { [(Located, Key, TypeU)] }
  : nam_entry_loc                              { [$1] }
  | nam_entry_list_loc VSEMI nam_entry_loc     { $1 ++ [$3] }

nam_entries :: { [(Key, TypeU)] }
  : nam_entry                          { [$1] }
  | nam_entries ',' nam_entry          { $1 ++ [$3] }

-- | RHS of a concrete (language-specific) typedef.
-- STRING-headed types are terminal (won't be resolved further).
-- Other types are non-terminal (will be resolved via the scope).
-- The two alternatives are disjoint: STRING can only match the first
-- (non_string_atom excludes STRING), so there are no LR conflicts.
concrete_rhs :: { (TypeU, Bool) }
  : STRING concrete_rhs_args    { (case $2 of { [] -> VarU (TV (getString $1)); ts -> AppU (VarU (TV (getString $1))) ts }, True) }
  | non_string_type             { ($1, False) }

concrete_rhs_args :: { [TypeU] }
  : {- empty -}                       { [] }
  | concrete_rhs_args atom_type       { $1 ++ [$2] }

-- | Type that does NOT start with a STRING token.
-- Used in concrete_rhs to avoid reduce/reduce conflicts with STRING.
non_string_type :: { TypeU }
  : non_string_non_fun '->' type  { case $3 of { FunU args ret -> FunU ($1 : args) ret; t -> FunU [$1] t } }
  | non_string_non_fun            { $1 }

non_string_non_fun :: { TypeU }
  : '<' LOWER '>'            { ExistU (TV (getName $2)) ([], Open) ([], Open) }
  | non_string_app            { $1 }

non_string_app :: { TypeU }
  : non_string_app atom_type  {% return (applyType $1 $2) }
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

typeclass_decl :: { ExprI }
  : 'class' class_head 'where' VLBRACE sig_list VRBRACE
      {% do { let (cs, cn, vs) = $2
            ; freshExprI $1 (ClsE (Typeclass cs cn vs $5)) } }
  | 'class' class_head
      {% do { let (cs, cn, vs) = $2
            ; freshExprI $1 (ClsE (Typeclass cs cn vs [])) } }

-- Parse class header, handling constraints via type reinterpretation.
-- Everything is parsed as app_type first, then reinterpreted.
-- If '=>' follows, the app_type was a constraint; otherwise it's the class head.
class_head :: { ([Constraint], ClassName, [TVar]) }
  : app_type '=>' app_type
      {% do { cs <- extractConstraints $1
            ; (cn, vs) <- extractClassDef $3
            ; return (cs, cn, vs) } }
  | '(' class_constraints ')' '=>' app_type
      {% do { (cn, vs) <- extractClassDef $5
            ; return ($2, cn, vs) } }
  | app_type
      {% do { (cn, vs) <- extractClassDef $1
            ; return ([], cn, vs) } }

class_constraints :: { [Constraint] }
  : single_constraint                            { [$1] }
  | class_constraints ',' single_constraint      { $1 ++ [$3] }

sig_list :: { [Signature] }
  : signature                     { [$1] }
  | sig_list VSEMI signature      { $1 ++ [$3] }

signature :: { Signature }
  : evar_or_op lower_names '::' sig_type
      {% let (cs, argDocs, t) = $4 in mkSignatureWithDocs $1 $2 cs argDocs t }

--------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------

instance_decl :: { ExprI }
  : 'instance' UPPER types1 'where' VLBRACE instance_items VRBRACE
      {% freshExprI $1 (IstE (ClassName (getName $2)) (map quantifyType $3) (concat $6)) }
  | 'instance' UPPER types1
      {% freshExprI $1 (IstE (ClassName (getName $2)) (map quantifyType $3) []) }

instance_items :: { [[ExprI]] }
  : instance_item                        { [$1] }
  | instance_items VSEMI instance_item   { $1 ++ [$3] }

instance_item :: { [ExprI] }
  : source_decl             { $1 }
  | sig_or_ass              { $1 }

--------------------------------------------------------------------
-- Fixity declarations
--------------------------------------------------------------------

fixity_decl :: { ExprI }
  : 'infixl' INTEGER operator_names
      {% freshExprI $1 (FixE (Fixity InfixL (fromInteger (getInt $2)) $3)) }
  | 'infixr' INTEGER operator_names
      {% freshExprI $1 (FixE (Fixity InfixR (fromInteger (getInt $2)) $3)) }
  | 'infix' INTEGER operator_names
      {% freshExprI $1 (FixE (Fixity InfixN (fromInteger (getInt $2)) $3)) }

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

source_decl :: { [ExprI] }
  : 'source' lang_name opt_from '(' source_items ')'
      {% mapM (mkSource $1 $2 $3) $5 }
  | 'source' lang_name opt_from 'where' VLBRACE source_new_items VRBRACE
      {% mapM (mkSourceNewWithDocs $1 $2 $3) $6 }

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

expr :: { ExprI }
  : let_expr                { $1 }
  | lambda_expr             { $1 }
  | infix_expr              { $1 }
  | infix_expr '::' type    {% freshExprI $2 (AnnE $1 (quantifyType $3)) }

let_expr :: { ExprI }
  : let_bindings 'in' expr
      {% freshExprI $2 (LetE $1 $3) }

let_bindings :: { [(EVar, ExprI)] }
  : let_binding                        { [$1] }
  | let_bindings let_binding           { $1 ++ [$2] }

let_binding :: { (EVar, ExprI) }
  : 'let' LOWER '=' expr              { (EV (getName $2), $4) }
  | 'let' '_' '=' expr                { (EV "_", $4) }

lambda_expr :: { ExprI }
  : '\\' lower_names1 '->' expr
      {% freshExprI $1 (LamE (map EV $2) $4) }

infix_expr :: { ExprI }
  : operand                  { $1 }
  | operand operator_name expr
      {% do { opI <- freshId $2
            ; freshExprI $2 (BopE $1 opI (EV (getOp $2)) $3) } }
  | operand '-' expr
      {% do { opI <- freshId $2
            ; freshExprI $2 (BopE $1 opI (EV "-") $3) } }
  | operand '.' expr
      {% do { opI <- freshId $2
            ; freshExprI $2 (BopE $1 opI (EV ".") $3) } }

operand :: { ExprI }
  : app_expr                 { $1 }
  | '-' INTEGER              {% freshExprI $1 (IntE (negate (getInt $2))) }
  | '-' FLOAT                {% freshExprI $1 (RealE (DS.fromFloatDigits (negate (getFloat $2)))) }

-- Function application: function followed by one or more arguments
app_expr :: { ExprI }
  : atom_expr                      { $1 }
  | atom_expr atom_exprs1          {% freshExprIFrom $1 (AppE $1 $2) }

atom_exprs1 :: { [ExprI] }
  : atom_expr                      { [$1] }
  | atom_exprs1 atom_expr          { $1 ++ [$2] }

atom_expr :: { ExprI }
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

force_expr :: { ExprI }
  : '!' atom_expr             {% freshExprI $1 (ForceE $2) }

paren_expr :: { ExprI }
  : '(' ')'                   {% freshExprI $1 UniE }
  | '(' operator_name ')'     {% freshExprI $1 (VarE defaultValue (EV (getOp $2))) }
  | '(' '-' ')'               {% freshExprI $1 (VarE defaultValue (EV "-")) }
  | '(' '.' ')'               {% freshExprI $1 (VarE defaultValue (EV ".")) }
  | '(' expr ')'              { $2 }
  | '(' expr ',' expr_list1 ')' {% freshExprI $1 (TupE ($2 : $4)) }

expr_list1 :: { [ExprI] }
  : expr                       { [$1] }
  | expr_list1 ',' expr        { $1 ++ [$3] }

-- Suspend and record both use real braces {}.
-- Disambiguated by LR(1): record entries start with LOWER '='.
-- Since '=' is not a valid infix operator, the parser can distinguish.
suspend_expr :: { ExprI }
  : '{' expr '}'              {% freshExprI $1 (SuspendE $2) }

record_expr :: { ExprI }
  : '{' record_entries '}'    {% freshExprI $1 (NamE $2) }

record_entries :: { [(Key, ExprI)] }
  : record_entry                         { [$1] }
  | record_entries ',' record_entry      { $1 ++ [$3] }

record_entry :: { (Key, ExprI) }
  : LOWER '=' expr                       { (Key (getName $1), $3) }

list_expr :: { ExprI }
  : '[' ']'                   {% freshExprI $1 (LstE []) }
  | '[' expr_list1 ']'        {% freshExprI $1 (LstE $2) }

do_expr :: { ExprI }
  : 'do' VLBRACE do_stmts VRBRACE     {% do { body <- desugarDo $1 $3; freshExprI $1 (SuspendE body) } }

do_stmts :: { [DoStmt] }
  : do_stmt                   { [$1] }
  | do_stmts VSEMI do_stmt    { $1 ++ [$3] }

do_stmt :: { DoStmt }
  : LOWER '<-' expr            { DoBind (EV (getName $1)) $3 }
  | expr                       { DoBare $1 }

getter_expr :: { ExprI }
  : GDOT accessor_body    {% buildAccessor $1 $2 }

-- After the initial GDOT, an accessor body is a field name, index, or group
accessor_body :: { AccessorBody }
  : LOWER accessor_tail           { ABKey (getName $1) $2 }
  | INTEGER accessor_tail         { ABIdx (fromInteger (getInt $1)) $2 }
  | '(' grouped_accessors ')'    { ABGroup $2 }

-- What follows a field name/index: nothing (getter), = expr (setter), or chain
accessor_tail :: { AccessorTail }
  : {- empty -}                   { ATEnd }
  | '=' expr                      { ATSet $2 }
  | GDOT accessor_body            { ATChain $2 }

-- Comma-separated entries inside .()
grouped_accessors :: { [AccessorBody] }
  : grouped_accessor                          { [$1] }
  | grouped_accessors ',' grouped_accessor   { $1 ++ [$3] }

-- Each entry inside .() starts with GDOT
grouped_accessor :: { AccessorBody }
  : GDOT accessor_body   { $2 }

var_expr :: { ExprI }
  : LOWER                     {% freshExprI $1 (VarE defaultValue (EV (getName $1))) }

hole_expr :: { ExprI }
  : '_'                        {% freshExprI $1 HolE }

bool_expr :: { ExprI }
  : 'True'                     {% freshExprI $1 (LogE True) }
  | 'False'                    {% freshExprI $1 (LogE False) }

num_expr :: { ExprI }
  : INTEGER                    {% freshExprI $1 (IntE (getInt $1)) }
  | FLOAT                      {% freshExprI $1 (RealE (DS.fromFloatDigits (getFloat $1))) }

string_expr :: { ExprI }
  : STRING                     {% freshExprI $1 (StrE (getString $1)) }
  | interp_string              { $1 }

interp_string :: { ExprI }
  : STRSTART interp_body STREND
      {% mkInterpString $1 $2 $3 }

interp_body :: { ([ExprI], [Text]) }
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

-- Left-recursive type application (like expr application).
-- This avoids the shift/reduce conflict that UPPER atom_types1 causes:
-- after seeing UPPER, the parser can't tell if it's a standalone type
-- or the head of a type application without arbitrary lookahead.
app_type :: { TypeU }
  : app_type atom_type        {% return (applyType $1 $2) }
  | atom_type                 { $1 }

atom_type :: { TypeU }
  : '(' ')'                  { BT.unitU }
  | '(' type ')'             { $2 }
  | '(' type ',' type_list1 ')' { BT.tupleU ($2 : $4) }
  | '[' type ']'              { BT.listU $2 }
  | '{' type '}'              { ThunkU $2 }
  | UPPER                     { VarU (TV (getName $1)) }
  | LOWER ':' non_fun_type   { $3 }  -- tag (e.g., x:Real), discard the tag
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

-- | A type with optional leading constraints, returning per-arg doc positions.
-- Both alternatives start with sig_fun_args to avoid reduce/reduce conflicts
-- between pos_atom_type and atom_type. When '=>' follows, the first sig_fun_args
-- is reinterpreted as constraints.
sig_type :: { ([Constraint], [ArgDocVars], TypeU) }
  : sig_fun_args '=>' sig_fun_args
      {% do { cs <- extractConstraints (argsToType $1)
            ; argDocs <- mapM (\(pos, _) -> lookupDocsPos pos) $3
            ; return (cs, map processArgDocLines argDocs, argsToType $3) } }
  | sig_fun_args
      {% do { argDocs <- mapM (\(pos, _) -> lookupDocsPos pos) $1
            ; return ([], map processArgDocLines argDocs, argsToType $1) } }

-- | Parse a function type as a list of positioned arrow-separated types.
-- Each entry is (position of first token, type).
sig_fun_args :: { [(Pos, TypeU)] }
  : pos_non_fun_type '->' sig_fun_args  { $1 : $3 }
  | pos_non_fun_type                     { [$1] }

-- | Non-function type with position of first token.
pos_non_fun_type :: { (Pos, TypeU) }
  : '<' LOWER '>'     { (locPos $1, ExistU (TV (getName $2)) ([], Open) ([], Open)) }
  | pos_app_type       { $1 }

-- | Type application with position of first token.
pos_app_type :: { (Pos, TypeU) }
  : pos_app_type atom_type  {% return (fst $1, applyType (snd $1) $2) }
  | pos_atom_type            { $1 }

-- | Atom type with position of first token.
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

-- Operator token (returns the Located for position tracking)
-- Note: '-' is NOT included here because it conflicts with unary negation
-- and infix subtraction. It's handled specifically in operator_ref,
-- evar_or_op, and paren_expr.
operator_name :: { Located }
  : OPERATOR                  { $1 }
  | '*'                       { $1 }
  | '<'                       { $1 }
  | '>'                       { $1 }

-- Located token that is evar or op (for sig_or_ass disambiguation)
evar_or_op :: { Located }
  : LOWER                     { $1 }
  | '(' operator_name ')'     { $2 }
  | '(' '-' ')'               { $2 }
  | '(' '.' ')'               { $2 }

opt_where_decls :: { [ExprI] }
  : {- empty -}                               { [] }
  | 'where' VLBRACE where_items VRBRACE       { $3 }

where_items :: { [ExprI] }
  : where_item                      { $1 }
  | where_items VSEMI where_item    { $1 ++ $3 }

where_item :: { [ExprI] }
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

data ParseError = ParseError
  { pePos      :: !Pos
  , peMsg      :: !String
  , peExpected :: ![String]
  , peSourceLines :: ![Text]
  }
  deriving (Show)

showParseError :: String -> ParseError -> String
showParseError filename (ParseError pos msg expected srcLines) =
  let ln = posLine pos
      col = posCol pos
      header = filename ++ ":" ++ show ln ++ ":" ++ show col ++ ": " ++ msg
      context = formatSourceContext srcLines ln col
      expectMsg = case cleanExpected expected of
        [] -> ""
        [x] -> "\n  expected " ++ x
        xs -> "\n  expected one of: " ++ intercalate ", " xs
  in header ++ context ++ expectMsg

formatSourceContext :: [Text] -> Int -> Int -> String
formatSourceContext srcLines ln col
  | ln < 1 || ln > length srcLines = ""
  | otherwise =
    let srcLine = srcLines !! (ln - 1)
        lineNum = show ln
        pad = replicate (length lineNum) ' '
        pointer = replicate (col - 1) ' ' ++ "^"
    in "\n  " ++ pad ++ " |\n  " ++ lineNum ++ " | " ++ T.unpack srcLine ++ "\n  " ++ pad ++ " | " ++ pointer

cleanExpected :: [String] -> [String]
cleanExpected = filter (not . isInternal) . nub . map friendlyName
  where
    isInternal s = s `elem` ["VLBRACE", "VRBRACE", "VSEMI", "EOF"]
    friendlyName "LOWER"      = "identifier"
    friendlyName "UPPER"      = "type name"
    friendlyName "OPERATOR"   = "operator"
    friendlyName "INTEGER"    = "integer"
    friendlyName "FLOAT"      = "number"
    friendlyName "STRING"     = "string"
    friendlyName "STRSTART"   = "string"
    friendlyName "STRMID"     = "string"
    friendlyName "STREND"     = "string"
    friendlyName "INTERPOPEN" = "'#{'"
    friendlyName "INTERPCLOSE" = "'}'"
    friendlyName "GDOT"       = "'.'"
    friendlyName s            = s

data PState = PState
  { psExpIndex    :: !Int
  , psSourceMap   :: !(Map.Map Int SrcLoc)
  , psModulePath  :: !(Maybe Path)
  , psModuleConfig :: !ModuleConfig
  , psDocMap      :: !(Map.Map Pos [Text])
  , psSourceLines :: ![Text]
  }
  deriving (Show)

emptyPState :: PState
emptyPState = PState 1 Map.empty Nothing defaultValue Map.empty []

type P a = State.StateT PState (Either ParseError) a

--------------------------------------------------------------------
-- Token extraction helpers
--------------------------------------------------------------------

-- | Extract name from identifier tokens
getName :: Located -> Text
getName (Located _ (TokLowerName n) _) = n
getName (Located _ (TokUpperName n) _) = n
getName (Located _ _ t) = t

-- | Extract integer value
getInt :: Located -> Integer
getInt (Located _ (TokInteger n) _) = n
getInt _ = 0

-- | Extract float value
getFloat :: Located -> Double
getFloat (Located _ (TokFloat d) _) = d
getFloat _ = 0

-- | Extract string value
getString :: Located -> Text
getString (Located _ (TokString s) _) = s
getString (Located _ (TokStringStart s) _) = s
getString (Located _ (TokStringMid s) _) = s
getString (Located _ (TokStringEnd s) _) = s
getString (Located _ _ t) = t

-- | Extract operator text from operator-like tokens
getOp :: Located -> Text
getOp (Located _ (TokOperator t) _) = t
getOp (Located _ TokMinus _) = "-"
getOp (Located _ TokStar _) = "*"
getOp (Located _ TokDot _) = "."
getOp (Located _ TokLAngle _) = "<"
getOp (Located _ TokRAngle _) = ">"
getOp (Located _ _ t) = t

-- | Get the name from a Located token as a Symbol
symVal :: Located -> Symbol
symVal (Located _ (TokLowerName n) _) = TermSymbol (EV n)
symVal (Located _ (TokUpperName n) _) = TypeSymbol (TV n)
symVal (Located _ (TokOperator n) _) = TermSymbol (EV n)
symVal (Located _ TokMinus _) = TermSymbol (EV "-")
symVal (Located _ TokStar _) = TermSymbol (EV "*")
symVal (Located _ TokDot _) = TermSymbol (EV ".")
symVal (Located _ TokLAngle _) = TermSymbol (EV "<")
symVal (Located _ TokRAngle _) = TermSymbol (EV ">")
symVal _ = TermSymbol (EV "?")

-- | Convert Located to EVar
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
-- Expression ID generation
--------------------------------------------------------------------

-- | Generate a fresh expression ID and record source position
freshId :: Located -> P Int
freshId tok = do
  s <- State.get
  let i = psExpIndex s
      p = locPos tok
      loc = SrcLoc (Just (posFile p)) (posLine p) (posCol p) (posLine p) (posCol p)
  State.put s { psExpIndex = i + 1
              , psSourceMap = Map.insert i loc (psSourceMap s) }
  return i

-- | Create a fresh ExprI with position from a Located token
freshExprI :: Located -> Expr -> P ExprI
freshExprI tok e = do
  i <- freshId tok
  return (ExprI i e)

-- | Create a fresh ExprI copying position from an existing ExprI
freshExprIFrom :: ExprI -> Expr -> P ExprI
freshExprIFrom (ExprI refId _) e = do
  s <- State.get
  let i = psExpIndex s
      loc = Map.findWithDefault noSrcLoc refId (psSourceMap s)
  State.put s { psExpIndex = i + 1
              , psSourceMap = Map.insert i loc (psSourceMap s) }
  return (ExprI i e)

noSrcLoc :: SrcLoc
noSrcLoc = SrcLoc Nothing 0 0 0 0

--------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------

-- | Look up docstrings for a token's position from the doc map
lookupDocs :: Located -> P [Text]
lookupDocs (Located pos _ _) = lookupDocsPos pos

-- | Look up docstrings by position
lookupDocsPos :: Pos -> P [Text]
lookupDocsPos pos = do
  docMap <- State.gets psDocMap
  return (Map.findWithDefault [] pos docMap)

-- | Parse a single docstring line like " name: foo" into key-value pairs
parseDocKV :: Text -> (Text, Text)
parseDocKV txt =
  let stripped = T.strip txt
  in case T.breakOn ":" stripped of
    (key, rest)
      | not (T.null rest) && not (T.any (== ' ') (T.strip key)) ->
        (T.strip key, T.strip (T.drop 1 rest))
    _ -> ("", stripped)

-- | Parse a CLI option string like "-n/--records" or "--verbose" or "-t"
parseCliOpt :: Text -> Maybe CliOpt
parseCliOpt txt = case T.unpack (T.strip txt) of
  '-' : '-' : rest@(_:_) -> Just (CliOptLong (T.pack rest))
  '-' : c : '/' : '-' : '-' : rest@(_:_) -> Just (CliOptBoth c (T.pack rest))
  '-' : c : [] -> Just (CliOptShort c)
  _ -> Nothing

-- | Parse docstring lines into ArgDocVars
processArgDocLines :: [Text] -> ArgDocVars
processArgDocLines = foldl processLine defaultValue
  where
    processLine d line = case parseDocKV line of
      ("name", val) -> d { docName = Just val }
      ("literal", val) -> d { docLiteral = Just (val == "true" || val == "True") }
      ("unroll", val) -> d { docUnroll = Just (val == "true" || val == "True") }
      ("default", val) -> d { docDefault = Just val }
      ("metavar", val) -> d { docMetavar = Just val }
      ("arg", val) -> d { docArg = parseCliOpt val }
      ("true", val) -> d { docTrue = parseCliOpt val }
      ("false", val) -> d { docFalse = parseCliOpt val }
      ("return", val) -> d { docReturn = Just val }
      (_, val) | not (T.null val) -> d { docLines = docLines d <> [val] }
      _ -> d

-- | Apply docstrings to a Source record
applySourceDocs :: [Text] -> Source -> Source
applySourceDocs docLines' src = foldl processLine src docLines'
  where
    processLine s line = case parseDocKV line of
      ("name", val) -> s { srcName = SrcName val }
      ("rsize", val) -> s { srcRsize = mapMaybe readMaybeInt (T.words val) }
      (_, val) | not (T.null val) -> s { srcNote = srcNote s <> [val] }
      _ -> s
    readMaybeInt t = case reads (T.unpack t) of
      [(n, "")] -> Just n
      _ -> Nothing

-- | Wrap type variables in forall
forallWrap :: [TVar] -> TypeU -> TypeU
forallWrap [] t = t
forallWrap (v : vs) t = ForallU v (forallWrap vs t)

-- | Wrap free lowercase type variables in ForallU (like pTypeGen in old parser)
quantifyType :: TypeU -> TypeU
quantifyType t = forallWrap (nub (collectGenVars t)) t
  where
    collectGenVars :: TypeU -> [TVar]
    collectGenVars (VarU v@(TV name))
      | not (T.null name), isLower (T.head name) = [v]
      | otherwise = []
    collectGenVars (ForallU v inner) = filter (/= v) (collectGenVars inner)
    collectGenVars (AppU f args) = collectGenVars f ++ concatMap collectGenVars args
    collectGenVars (FunU args ret) = concatMap collectGenVars args ++ collectGenVars ret
    collectGenVars (NamU _ _ ts entries) = concatMap collectGenVars ts ++ concatMap (collectGenVars . snd) entries
    collectGenVars (ThunkU inner) = collectGenVars inner
    collectGenVars _ = []

-- | Parse a language name
parseLang :: Text -> P Lang
parseLang t = case ML.readLangName t of
  Just lang -> return lang
  Nothing -> pfail (Pos 0 0 "") ("unknown language: " ++ T.unpack t)

pfail :: Pos -> String -> P a
pfail pos msg = do
  srcLines <- State.gets psSourceLines
  State.lift (Left (ParseError pos msg [] srcLines))

-- | Build interpolated string expression
mkInterpString :: Located -> ([ExprI], [Text]) -> Located -> P ExprI
mkInterpString startTok (exprs, mids) endTok = do
  let tails = mids ++ [getString endTok]
  patI <- freshExprI startTok (PatE (PatternText (getString startTok) tails))
  freshExprI startTok (AppE patI exprs)

-- | Flatten left-recursive application into AppE f [args]

-- | Flatten left-recursive type application into AppU head [args]
applyType :: TypeU -> TypeU -> TypeU
applyType (AppU f args) x = AppU f (args ++ [x])
applyType f x = AppU f [x]

-- | Reinterpret a type that was parsed before '=>' as constraints.
-- Handles: Ord a, (Ord a, Show a), (Ord a)
extractConstraints :: TypeU -> P [Constraint]
extractConstraints (AppU (VarU (TV name)) args) =
  return [Constraint (ClassName name) args]
extractConstraints (VarU (TV name)) =
  return [Constraint (ClassName name) []]
extractConstraints (NamU NamRecord _ _ entries) =
  -- Tuple types parsed as (A a, B b) become tuple-like structures.
  -- We need to handle tuples which use BT.tupleU.
  pfail (Pos 0 0 "") "invalid constraint syntax"
extractConstraints t =
  -- Try to extract from tuple representation (Tuple2 (Ord a) (Show a) etc.)
  case flattenTupleConstraint t of
    Just cs -> return cs
    Nothing -> pfail (Pos 0 0 "") ("invalid constraint: " ++ show t)

flattenTupleConstraint :: TypeU -> Maybe [Constraint]
flattenTupleConstraint (AppU (VarU (TV name)) args)
  | T.isPrefixOf "Tuple" name = mapM typeToConstraint args
  | otherwise = Just [Constraint (ClassName name) args]
flattenTupleConstraint (VarU (TV name)) =
  Just [Constraint (ClassName name) []]
flattenTupleConstraint _ = Nothing

-- | Extract class name and type variables from a type parsed as app_type
extractClassDef :: TypeU -> P (ClassName, [TVar])
extractClassDef (AppU (VarU (TV name)) args) = do
  tvs <- mapM typeToTVar args
  return (ClassName name, tvs)
extractClassDef (VarU (TV name)) =
  return (ClassName name, [])
extractClassDef _ = pfail (Pos 0 0 "") "invalid class head"

typeToTVar :: TypeU -> P TVar
typeToTVar (VarU tv) = return tv
typeToTVar _ = pfail (Pos 0 0 "") "expected type variable in class head"

typeToConstraint :: TypeU -> Maybe Constraint
typeToConstraint (AppU (VarU (TV name)) args) =
  Just (Constraint (ClassName name) args)
typeToConstraint (VarU (TV name)) =
  Just (Constraint (ClassName name) [])
typeToConstraint _ = Nothing

--------------------------------------------------------------------
-- Semantic action helpers
--------------------------------------------------------------------

mkSignatureWithDocs :: Located -> [Text] -> [Constraint] -> [ArgDocVars] -> TypeU -> P Signature
mkSignatureWithDocs nameTok vs cs argDocs t = do
  let wrappedT = forallWrap (map TV vs) t
      doc = ArgDocSig defaultValue (init argDocs) (last argDocs)
      et = EType wrappedT (Set.fromList cs) doc
  return (Signature (toEVar nameTok) Nothing et)

-- | Convert a list of positioned types to a TypeU (FunU or single type)
argsToType :: [(Pos, TypeU)] -> TypeU
argsToType [] = BT.unitU
argsToType [(_, t)] = t
argsToType ts = FunU (map snd (init ts)) (snd (last ts))

mkTypedef :: Located -> Maybe (Lang, Bool) -> (TVar, [Either TVar TypeU]) -> TypeU -> P ExprI
mkTypedef tok lang (v, vs) t = do
  docs <- lookupDocs tok
  let docVars = if null docs then defaultValue else processArgDocLines docs
  freshExprI tok (TypE (ExprTypeE lang v vs t (ArgDocAlias docVars)))

mkTypedefAlias :: Located -> (TVar, [Either TVar TypeU]) -> P ExprI
mkTypedefAlias tok (v, vs) =
  let t = if null vs then VarU v else AppU (VarU v) (map (either VarU id) vs)
  in freshExprI tok (TypE (ExprTypeE Nothing v vs t (ArgDocAlias defaultValue)))

mkNamTypeWhereWithDocs :: Located -> Located -> NamType -> (TVar, [Either TVar TypeU]) -> [(Located, Key, TypeU)] -> P ExprI
mkNamTypeWhereWithDocs recTok tok nt (v, vs) locEntries = do
  recDocs <- lookupDocs recTok
  let recDocVars = processArgDocLines recDocs
  fieldDocs <- mapM (\(loc, _, _) -> do { dl <- lookupDocs loc; return (processArgDocLines dl) }) locEntries
  let entries = [(k, t) | (_, k, t) <- locEntries]
      entries' = desugarTableEntries nt entries
      doc = ArgDocRec recDocVars (zip (map fst entries') fieldDocs)
      t = NamU nt v (map (either VarU id) vs) entries'
  freshExprI tok (TypE (ExprTypeE Nothing v vs t doc))

mkNamTypeLegacy :: Located -> NamType -> (TVar, [Either TVar TypeU]) -> Maybe Text -> Maybe (Lang, Bool) -> [(Key, TypeU)] -> P ExprI
mkNamTypeLegacy tok nt (v, vs) mayCon lang entries =
  let con = maybe v TV mayCon
      entries' = desugarTableEntries nt entries
      t = NamU nt con (map (either VarU id) vs) entries'
      doc = ArgDocRec defaultValue [(k, defaultValue) | (k, _) <- entries']
  in freshExprI tok (TypE (ExprTypeE lang v vs t doc))

-- | For table types, wrap each field type in a list type
desugarTableEntries :: NamType -> [(Key, TypeU)] -> [(Key, TypeU)]
desugarTableEntries NamTable entries = [(k, wrapList t) | (k, t) <- entries]
  where
    wrapList (ForallU v t) = ForallU v (wrapList t)
    wrapList t = BT.listU t
desugarTableEntries _ entries = entries


-- | Resolve a source file path relative to the current module's directory
resolveSourceFile :: Maybe Path -> Maybe Text -> Maybe Path
resolveSourceFile modulePath srcFile =
  case (modulePath, srcFile) of
    (Just f, Just srcfile') -> Just $ combine (takeDirectory f) (T.unpack srcfile')
    (Just _, Nothing) -> Nothing
    (Nothing, s) -> fmap T.unpack s

mkSource :: Located -> Lang -> Maybe Text -> (Text, Maybe Text) -> P ExprI
mkSource tok lang srcfile (name, mayAlias) = do
  modPath <- State.gets psModulePath
  let alias = maybe name id mayAlias
      path = resolveSourceFile modPath srcfile
  freshExprI tok (SrcE Source
    { srcName = SrcName name
    , srcLang = lang
    , srcPath = path
    , srcAlias = EV alias
    , srcLabel = Nothing
    , srcRsize = []
    , srcNote = []
    })

mkSourceNewWithDocs :: Located -> Lang -> Maybe Text -> Located -> P ExprI
mkSourceNewWithDocs tok lang srcfile nameTok = do
  modPath <- State.gets psModulePath
  docLines' <- lookupDocs nameTok
  let name = getName nameTok
      path = resolveSourceFile modPath srcfile
      baseSrc = Source
        { srcName = SrcName name
        , srcLang = lang
        , srcPath = path
        , srcAlias = EV name
        , srcLabel = Nothing
        , srcRsize = []
        , srcNote = []
        }
      src = applySourceDocs docLines' baseSrc
  freshExprI tok (SrcE src)

-- | Accessor tree types
data AccessorBody
  = ABKey Text AccessorTail    -- .name<tail>
  | ABIdx Int AccessorTail     -- .0<tail>
  | ABGroup [AccessorBody]     -- .(<entries>)

data AccessorTail
  = ATEnd                      -- nothing after field (getter)
  | ATSet ExprI                -- = expr (setter)
  | ATChain AccessorBody       -- .next (continue chain)

-- | Result of resolving an accessor tree
data AccessorResult
  = ARGetter Selector
  | ARSetter Selector [ExprI]  -- selector (with group structure) + values

-- | Build a getter or setter expression from an accessor tree
buildAccessor :: Located -> AccessorBody -> P ExprI
buildAccessor tok body = do
  result <- resolveBody body
  case result of
    ARGetter sel -> freshExprI tok (PatE (PatternStruct sel))
    ARSetter sel vals -> do
      patI <- freshExprI tok (PatE (PatternStruct sel))
      lamI <- freshId tok
      let v = EV (".setter_" <> T.pack (show lamI))
      vArg <- freshExprI tok (VarE defaultValue v)
      appI <- freshExprI tok (AppE patI (vArg : vals))
      return (ExprI lamI (LamE [v] appI))

resolveBody :: AccessorBody -> P AccessorResult
resolveBody (ABKey name tail') = do
  inner <- resolveTail tail'
  return (wrapKey name inner)
resolveBody (ABIdx idx tail') = do
  inner <- resolveTail tail'
  return (wrapIdx idx inner)
resolveBody (ABGroup entries) = resolveGroup entries

resolveTail :: AccessorTail -> P AccessorResult
resolveTail ATEnd = return (ARGetter SelectorEnd)
resolveTail (ATSet expr) = return (ARSetter SelectorEnd [expr])
resolveTail (ATChain body) = resolveBody body

wrapKey :: Text -> AccessorResult -> AccessorResult
wrapKey name (ARGetter sel) = ARGetter (SelectorKey (name, sel) [])
wrapKey name (ARSetter sel vals) = ARSetter (SelectorKey (name, sel) []) vals

wrapIdx :: Int -> AccessorResult -> AccessorResult
wrapIdx idx (ARGetter sel) = ARGetter (SelectorIdx (idx, sel) [])
wrapIdx idx (ARSetter sel vals) = ARSetter (SelectorIdx (idx, sel) []) vals

resolveGroup :: [AccessorBody] -> P AccessorResult
resolveGroup bodies = do
  results <- mapM resolveBody bodies
  let getters = [s | ARGetter s <- results]
      setterPairs = [(s, vs) | ARSetter s vs <- results]
  case (getters, setterPairs) of
    (gs, []) -> return (ARGetter (mergeSelectors gs))
    ([], ss) -> return (ARSetter (mergeSelectors (map fst ss)) (concatMap snd ss))
    _ -> pfail (Pos 0 0 "") "cannot mix getter and setter entries in .()"

-- | Merge multiple selectors into one with extras (for groups)
mergeSelectors :: [Selector] -> Selector
mergeSelectors [] = SelectorEnd
mergeSelectors [s] = s
mergeSelectors sels =
  let idxEntries = concat [s : ss | SelectorIdx s ss <- sels]
      keyEntries = concat [s : ss | SelectorKey s ss <- sels]
  in case (idxEntries, keyEntries) of
    (is, []) -> case is of { [] -> SelectorEnd; (x:xs) -> SelectorIdx x xs }
    ([], ks) -> case ks of { [] -> SelectorEnd; (x:xs) -> SelectorKey x xs }
    _ -> error "Cannot mix key and index selectors in getter"

data DoStmt = DoBind EVar ExprI | DoBare ExprI

desugarDo :: Located -> [DoStmt] -> P ExprI
desugarDo tok [] = pfail (locPos tok) "empty do block"
desugarDo tok [DoBare e] = freshExprI tok (ForceE e)
desugarDo tok [DoBind _ _] = pfail (locPos tok) "do block cannot end with a bind (<-)"
desugarDo tok (DoBind v e : rest) = do
  forceE <- freshExprI tok (ForceE e)
  restE <- desugarDo tok rest
  freshExprI tok (LetE [(v, forceE)] restE)
desugarDo tok (DoBare e : rest) = do
  idx <- freshId tok
  let discardVar = EV ("_do_" <> T.pack (show idx))
  forceE <- freshExprI tok (ForceE e)
  restE <- desugarDo tok rest
  freshExprI tok (LetE [(discardVar, forceE)] restE)

-- | Wrap top-level declarations (without explicit module keyword) in an
-- implicit "main" module. Bare expressions are not supported at the top
-- level; use explicit assignments instead.
mkImplicitMain :: [ExprI] -> P [ExprI]
mkImplicitMain es = do
  let tok = Located (Pos 0 0 "") TokEOF ""
  modI <- freshId tok
  return [ExprI modI (ModE (MV "main") es)]

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
  case State.runStateT (parseProgram tokens) pstate' of
    Right (result, finalState) ->
      let dag' = foldl addModule dag result
      in return (dag', finalState)
    Left err ->
      -- Strategy 2: wrap in module, patch trailing expr as __expr__ assignment.
      -- Handles bare code like "f x = True\nf 42" where the last statement
      -- is an expression (not a declaration).
      let wrappedCode = "module main (*)\n" <> sourceCode
      in case lexMorloc filename wrappedCode of
        Right (wrappedTokens, wrappedDocMap) ->
          -- First try parsing the wrapped code directly (pure declarations)
          let pstate'' = pstate' { psDocMap = wrappedDocMap, psSourceLines = T.lines wrappedCode }
          in case State.runStateT (parseProgram wrappedTokens) pstate'' of
            Right (result, finalState) ->
              let dag' = foldl addModule dag result
              in return (dag', finalState)
            Left _ ->
              -- Patch: change export (*) to (__expr__) and insert __expr__ =
              -- before the last top-level statement
              case patchForTrailingExpr wrappedTokens of
                Just patchedTokens ->
                  case State.runStateT (parseProgram patchedTokens) pstate'' of
                    Right (result, finalState) ->
                      let dag' = foldl addModule dag result
                      in return (dag', finalState)
                    Left _ -> tryExprFallback tokens pstate' dag filename err
                Nothing -> tryExprFallback tokens pstate' dag filename err
        Left _ -> tryExprFallback tokens pstate' dag filename err
  where
    tryExprFallback tokens' ps dag' filename' origErr =
      -- Last resort: parse as a bare expression (e.g., "42", "True", "f 42").
      let exprTokens = stripLayoutTokens tokens'
      in case State.runStateT (parseExprOnly exprTokens) ps of
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

-- | Patch module-wrapped tokens to handle trailing bare expressions.
-- Changes export from (*) to (__expr__) and inserts "__expr__ =" before
-- the last top-level statement.
patchForTrailingExpr :: [Located] -> Maybe [Located]
patchForTrailingExpr tokens = do
  let tokens' = patchExport tokens
  patchLastStmt tokens'

-- | Replace (*) with (__expr__) in the export list
patchExport :: [Located] -> [Located]
patchExport [] = []
patchExport (t@(Located _ TokLParen _) : Located p TokStar _ : rest) =
  t : Located p (TokLowerName "__expr__") "__expr__" : rest
patchExport (t : rest) = t : patchExport rest

-- | Insert "__expr__ =" after the last top-level VSEMI
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

-- | Strip virtual layout tokens for expression fallback parsing
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
