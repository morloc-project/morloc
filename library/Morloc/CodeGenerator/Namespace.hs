{-|
Module      : Morloc.CodeGenerator.Namespace
Description : All code generator types and datastructures
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Namespace
  ( module Morloc.Namespace
  -- ** Types used in final translations
  , TypeM(..)
  , ExprM(..)
  , Argument(..)
  , JsonType(..)
  , PVar(..)
  , TypeP(..)
  , JsonPath
  , JsonAccessor(..)
  , NexusCommand(..)
  -- ** Serialization AST
  , SerialAST(..)
  , TypePacker(..)
  -- ** Accessors
  , argId
  ) where

import Morloc.Namespace
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Set as Set
import Morloc.Data.Doc
import Morloc.Pretty ()

-- | Stores the language, general name and concrete name for a type expression
data PVar
  = PV
    Lang
    (Maybe Text) -- ^ general name of the type variable (if known)
    Text -- ^ concrete name of type variable
  deriving (Show, Eq, Ord)

-- | A solved type coupling a language specific form to an optional general form
data TypeP
  = UnkP PVar
  | VarP PVar
  | FunP [TypeP] TypeP
  | AppP TypeP [TypeP]
  | NamP NamType PVar [PVar] [(PVar, TypeP)]
  deriving (Show, Ord, Eq)

type JsonPath = [JsonAccessor]
data JsonAccessor
  = JsonIndex Int
  | JsonKey Text

data NexusCommand = NexusCommand
  { commandName :: EVar -- ^ user-exposed subcommand name in the nexus
  , commandType :: Type -- ^ the general type of the expression
  , commandJson :: MDoc -- ^ JSON output with null's where values will be replaced
  , commandArgs :: [EVar] -- ^ list of function arguments
  , commandSubs :: [(JsonPath, Text, JsonPath)]
  -- ^ list of tuples with values 1) path in JSON to value needs to be replaced
  -- 2) the function argument from which to pull replacement value and 3) the
  -- path to the replacement value
  }

instance Typelike TypeP where
  typeOf (UnkP v) = UnkT (pvar2tvar v)
  typeOf (VarP v) = VarT (pvar2tvar v)
  typeOf (FunP ts t) = FunT (map typeOf ts) (typeOf t)
  typeOf (AppP v ts) = AppT (typeOf v) (map typeOf ts)
  typeOf (NamP o n ps es) =
    let n' = pvar2tvar n
        ps' = (map pvar2tvar ps)
        es' = [(v, typeOf t) | (PV _ _ v, t) <- es]
    in NamT o n' ps' es'

  -- | substitute all appearances of a given variable with a given new type
  substituteTVar v0 r0 t0 = sub t0
    where
      sub :: TypeP -> TypeP
      sub t@(UnkP _) = t
      sub t@(VarP (PV lang _ v))
        | v0 == TV (Just lang) v = r0
        | otherwise = t
      sub (FunP ts t) = FunP (map sub ts) (sub t)
      sub (AppP v ts) = AppP (sub v) (map sub ts)
      sub (NamP o n ps es) = NamP o n ps [(k, sub t) | (k, t) <- es]

  free v@(VarP _) = Set.singleton v
  free (FunP ts t) = Set.unions (map free (t:ts))
  free (AppP t ts) = Set.unions (map free (t:ts))
  free (NamP _ _ _ es) = Set.unions (map (free . snd) es)
  free (UnkP _) = Set.empty -- are UnkP free?

pvar2tvar :: PVar -> TVar
pvar2tvar (PV lang _ v) = TV (Just lang) v

-- | A tree describing how to (de)serialize an object
data SerialAST f
  = SerialPack PVar (f (TypePacker, SerialAST f)) -- ^ use an (un)pack function to simplify an object
  | SerialList (SerialAST f)
  | SerialTuple [SerialAST f]
  | SerialObject NamType PVar [PVar] [(PVar, SerialAST f)]
  -- ^ Make a record, table, or object. The parameters indicate
  --   1) NamType - record/table/object
  --   2) PVar - telling the name of the object (e.g., "Person")
  --   3) [PVar] - listing the generic metavariable names
  --   4) [(PVar, SerialAST f)] - entries with keys for concrete and general cases
  | SerialNum PVar
  | SerialBool PVar
  | SerialString PVar
  | SerialNull PVar
  | SerialUnknown PVar
  -- ^ depending on the language, this may or may not raise an error down the
  -- line, the parameter contains the variable name, which is useful only for
  -- source code comments.

instance Foldable f => Pretty (SerialAST f) where
  pretty (SerialPack v packers) = parens
    $ "SerialPack" <+> pretty v
    <+> braces (foldr (\(p, x) s -> s <+> tupled [pretty p, pretty x]) "" packers)
  pretty (SerialList ef) = parens $ "SerialList" <+> pretty ef 
  pretty (SerialTuple efs) = parens $ "SerialTuple" <+> tupled (map pretty efs)
  pretty (SerialObject o p vs rs) = parens
    $ "SerialObject" <+> pretty o <+> tupled (map pretty vs)
    <+> encloseSep "{" "}" "," [pretty k <+> "=" <+> pretty p | (k, p) <- rs]
  pretty (SerialNum v) = parens ("SerialNum" <+> pretty v)
  pretty (SerialBool v) = parens ("SerialBool" <+> pretty v)
  pretty (SerialString v) = parens ("SerialString" <+> pretty v)
  pretty (SerialNull v) = parens ("SerialNull" <+> pretty v)
  pretty (SerialUnknown v) = parens ("SerialUnknown" <+> pretty v)

data TypePacker = TypePacker
  { typePackerType    :: TypeP
  , typePackerFrom    :: TypeP
  , typePackerForward :: [Source]
  , typePackerReverse :: [Source]
  } deriving (Show, Ord, Eq)

instance Pretty TypePacker where
  pretty p = "TypePacker" <+> encloseSep "{" "}" ","
    [ "typePackerType" <+> "=" <+> pretty (typePackerType p)
    , "typePackerFrom" <+> "=" <+> pretty (typePackerFrom p)
    , "typePackerForward" <+> "=" <+> list (map pretty (typePackerForward p))
    , "typePackerReverse" <+> "=" <+> list (map pretty (typePackerReverse p))
    ]

-- | A simplified subset of the Type record where functions, existentials,
-- universals and language-specific info are removed
data JsonType
  = VarJ Text
  -- ^ {"int"}
  | ArrJ Text [JsonType]
  -- ^ {"list":["int"]}
  | NamJ Text [(Text, JsonType)]
  -- ^ {"Foo":{"bar":"A","baz":"B"}}
  deriving (Show, Ord, Eq)

-- | An argument that is passed to a manifold
data Argument
  = SerialArgument Int TypeP
  -- ^ A serialized (e.g., JSON string) argument.  The parameters are 1)
  -- argument name (e.g., x), and 2) argument type (e.g., double). Some types
  -- may not be serializable. This is OK, so long as they are only used in
  -- functions of the same language.
  | NativeArgument Int TypeP
  -- ^ A native argument with the same parameters as above
  | PassThroughArgument Int
  -- ^ A serialized argument that is untyped in the current language. It cannot
  -- be deserialized, but will be passed eventually to a foreign argument where it
  -- does have a concrete type.
  deriving (Show, Ord, Eq)

argId :: Argument -> Int
argId (SerialArgument i _) = i
argId (NativeArgument i _) = i
argId (PassThroughArgument i ) = i

data TypeM
  = Passthrough -- ^ serialized data that cannot be deserialized in this language
  | Serial TypeP -- ^ serialized data that may be deserialized in this language
  | Native TypeP -- ^ an unserialized native data type
  | Function [TypeM] TypeM -- ^ a function of n inputs and one output (cannot be serialized)
  deriving(Show, Eq, Ord)


-- | A grammar that describes the implementation of the pools. Expressions in
-- this grammar will be directly translated into concrete code.
data ExprM f
  = ManifoldM Int [Argument] (ExprM f)
  -- ^ A wrapper around a single source call or (in some cases) a container.

  | ForeignInterfaceM
      TypeM -- required type in the calling language
      (ExprM f) -- expression in the foreign language
  -- ^ A generic interface to an expression in another language. Currently it
  -- will be resolved only to the specfic pool call interface type, where
  -- system calls pass serialized information between pools in different
  -- languages. Eventually, better methods will be added for certain pairs of
  -- languages.

  | PoolCallM
      TypeM -- serialized return data
      Int -- foreign manifold id
      [MDoc] -- shell command components that preceed the passed data
      [Argument] -- argument passed to the foreign function (must be serialized)
  -- ^ Make a system call to another language

  | LetM Int (ExprM f) (ExprM f)
  -- ^ let syntax allows fine control over order of operations in the generated
  -- code. The Int is an index for a LetVarM. It is also important in languages
  -- such as C++ where values need to be declared with explicit types and
  -- special constructors.

  | AppM
      (ExprM f) -- ManifoldM | SrcM | LamM
      [(ExprM f)]

  | SrcM TypeM Source
  -- ^ a within pool function call (cis)

  | LamM [Argument] (ExprM f)
  -- ^ Nothing Evar will be auto generated

  | BndVarM TypeM Int
  -- ^ A lambda-bound variable. BndVarM only describes variables bound as positional
  -- arguments in a manifold. The are represented as integers since the name
  -- will be language-specific.
  --
  -- In the rewrite step, morloc declarations are removed. So the expression:
  --   x = 5
  --   foo y = mul x y
  -- Is rewritten as:
  --   \y -> mul 5 y
  -- So BndVarM does NOT include variables defined in the morloc script. It only
  -- includes lambda-bound variables. The only BndVarM is `y` (`mul` is SrcM). The
  -- literal name "y" is replaced, though, with the integer 1. This is required in
  -- order to avoid name conflicts in concrete languages, for example consider
  -- the following (perfectly legal) morloc function:
  --   foo for = mul for 2
  -- If the string "for" were retained as the variable name, this would fail in
  -- many language where "for" is a keyword.

  | AccM (ExprM f) Text
  -- ^ Access a field in record ExprM

  | LetVarM TypeM Int
  -- ^ An internally generated variable id used in let assignments. When
  -- translated into a language, the integer will be used to generate a unique
  -- variable name (e.g. [a0,a1,...] or [a,b,c,...]).

  -- containers
  | ListM TypeM [(ExprM f)]
  | TupleM TypeM [(ExprM f)]
  | RecordM TypeM [(Text, (ExprM f))]

  -- primitives
  | LogM TypeM Bool
  | NumM TypeM Scientific
  | StrM TypeM Text
  | NullM TypeM

  -- serialization
  | SerializeM (SerialAST f) (ExprM f)
  | DeserializeM (SerialAST f) (ExprM f)

  | ReturnM (ExprM f)
  -- ^ The return value of a manifold. I need this to distinguish between the
  -- values assigned in let expressions and the final return value. In some
  -- languages, this may not be necessary (e.g., R).


instance HasOneLanguage (TypeP) where
  langOf' (UnkP (PV lang _ _)) = lang
  langOf' (VarP (PV lang _ _)) = lang
  langOf' (FunP _ t) = langOf' t 
  langOf' (AppP t _) = langOf' t
  langOf' (NamP _ (PV lang _ _) _ _) = lang


instance HasOneLanguage (TypeM) where
  langOf Passthrough = Nothing 
  langOf (Serial t) = langOf t
  langOf (Native t) = langOf t
  langOf (Function _ t) = langOf t


instance HasOneLanguage (ExprM f) where
  -- langOf :: a -> Maybe Lang
  langOf' (ManifoldM _ _ e) = langOf' e
  langOf' (ForeignInterfaceM t _) = langOf' t
  langOf' (PoolCallM t _ _ _) = langOf' t
  langOf' (LetM _ _ e2) = langOf' e2
  langOf' (AppM e _) = langOf' e
  langOf' (SrcM _ src) = srcLang src
  langOf' (LamM _ e) = langOf' e
  langOf' (BndVarM t _) = langOf' t
  langOf' (LetVarM t _) = langOf' t
  langOf' (AccM e _) = langOf' e
  langOf' (ListM t _) = langOf' t
  langOf' (TupleM t _) = langOf' t
  langOf' (RecordM t _) = langOf' t
  langOf' (LogM t _) = langOf' t
  langOf' (NumM t _) = langOf' t
  langOf' (StrM t _) = langOf' t
  langOf' (NullM t) = langOf' t
  langOf' (SerializeM _ e) = langOf' e
  langOf' (DeserializeM _ e) = langOf' e
  langOf' (ReturnM e) = langOf' e


instance Pretty Argument where
  pretty (SerialArgument i c) =
    "Serial" <+> "x" <> pretty i <+> parens (pretty c)
  pretty (NativeArgument i c) =
    "Native" <+> "x" <> pretty i <+> parens (pretty c)
  pretty (PassThroughArgument i) =
    "PassThrough" <+> "x" <> pretty i

instance Pretty (ExprM f) where
  pretty e0 = (vsep . punctuate line . fst $ f e0) <> line where
    manNamer i = "m" <> pretty i

    f (ManifoldM m args e) =
      let (ms', body) = f e
          decl = manNamer m <> tupled (map pretty args)
          mdoc = block 4 decl body
      in (mdoc : ms', manNamer m)
    f (PoolCallM t _ cmds args) =
      let poolArgs = cmds ++ map pretty args
      in ([], "PoolCallM" <> list (map (pretty . render) poolArgs) <+> "::" <+> pretty t)
    f (ForeignInterfaceM t e) =
      let (ms, _) = f e
      in (ms, "ForeignInterface :: " <> pretty t)
    f (LetM v e1 e2) =
      let (ms1', e1') = f e1
          (ms2', e2') = f e2
      in (ms1' ++ ms2', "a" <> pretty v <+> "=" <+> e1' <> line <> e2')
    f (AppM fun xs) =
      let (ms', fun') = f fun
          (mss', xs') = unzip $ map f xs
      in (ms' ++ concat mss', fun' <> tupled xs')
    f (SrcM _ src) = ([], pretty (srcName src))
    f (LamM args e) =
      let (ms', e') = f e
          vsFull = map pretty args
          vsNames = map (\r -> "x" <> pretty (argId r)) args
      in (ms', "\\ " <+> hsep (punctuate "," vsFull) <> "->" <+> e' <> tupled vsNames)
    f (BndVarM _ i) = ([], "x" <> pretty i)
    f (LetVarM _ i) = ([], "a" <> pretty i)
    f (AccM e k) =
      let (ms, e') = f e
      in (ms, parens e' <> "@" <> pretty k)
    f (ListM _ es) =
      let (mss', es') = unzip $ map f es
      in (concat mss', list es')
    f (TupleM _ es) =
      let (mss', es') = unzip $ map f es
      in (concat mss', tupled es')
    f (RecordM c entries) =
      let (mss', es') = unzip $ map (f . snd) entries
          entries' = zipWith (\k v -> pretty k <> "=" <> v) (map fst entries) es'
      in (concat mss', prettyRecordPVar c <+> "{" <> tupled entries' <> "}")
    f (LogM _ x) = ([], if x then "true" else "false")
    f (NumM _ x) = ([], viaShow x)
    f (StrM _ x) = ([], dquotes $ pretty x)
    f (NullM _) = ([], "null")
    f (SerializeM _ e) =
      let (ms, e') = f e
      in (ms, "PACK" <> tupled [e'])
    f (DeserializeM _ e) =
      let (ms, e') = f e
      in (ms, "UNPACK" <> tupled [e'])
    f (ReturnM e) =
      let (ms, e') = f e
      in (ms, "RETURN(" <> e' <> ")")

    prettyRecordPVar (Serial _) = "HELPME_ME_SERIAL_RECORD"
    prettyRecordPVar (Native _) = "HELPME_ME_NATIVE_RECORD"
    prettyRecordPVar _ = "<UNKNOWN RECORD>"

instance Pretty PVar where
  pretty (PV _ (Just g) t) = parens (pretty g <+> pretty t)
  pretty (PV _ Nothing t) = parens ("*" <+> pretty t)

instance Pretty TypeP where
  pretty (UnkP v) = pretty v 
  pretty (VarP v) = pretty v 
  pretty (FunP ts t) = encloseSep "(" ")" " -> " (map pretty (ts <> [t]))
  pretty (AppP t ts) = hsep (map pretty (t:ts))
  pretty (NamP o n ps rs)
      = block 4 (viaShow o <+> pretty n <> encloseSep "<" ">" "," (map pretty ps))
                (vsep [pretty k <+> "::" <+> pretty x | (k, x) <- rs])


instance Pretty TypeM where
  pretty Passthrough = "Passthrough"
  pretty (Serial c) = "Serial<" <> pretty c <> ">"
  pretty (Native c) = "Native<" <> pretty c <> ">"
  pretty (Function ts t) =
    "Function<" <> hsep (punctuate "->" (map pretty (ts ++ [t]))) <> ">"
