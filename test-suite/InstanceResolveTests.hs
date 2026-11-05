{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : InstanceResolveTests
-- Description : The instance table and 'resolve' on hand-built heads
module InstanceResolveTests (instanceResolveTests) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Morloc.Frontend.Instances
import Morloc.Namespace.Expr
import Morloc.Namespace.Prim
import Morloc.Namespace.Type
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

instanceResolveTests :: TestTree
instanceResolveTests =
  testGroup
    "instance resolution"
    [ testCase "alpha-equivalent heads are one key" $
        ( canonicalHead [ForallU a (w (VarU a))] == canonicalHead [ForallU b (w (VarU b))]
        , canonicalHead [ForallU a (vec n (VarU a))] == canonicalHead [ForallU b (vec (NatVarU (TV "m")) (VarU b))]
        , canonicalHead [ForallU a (pair (VarU a) int)] == canonicalHead [ForallU a (pair int (VarU a))] )
          @?= (True, True, False)
    , testCase "CLS-16-1 table: the literal Nat head wins where both match" $
        ( keyOf (run nat3 [] (sz (vec (NatLitU 3) int)))
        , keyOf (run nat3 [] (sz (vec (NatLitU 4) int))) )
          @?= (Just (key "Sz" [vec (NatLitU 3) int]), Just (key "Sz" [ForallU a (vec n (VarU a))]))
    , testCase "a Nat-variable head covers a literal use (N3)" $
        keyOf (run (env [decl "Sz" [] [ForallU a (vec n (VarU a))]]) [] (sz (vec (NatLitU 3) (VarU a))))
          @?= Just (key "Sz" [ForallU a (vec n (VarU a))])
    , testCase "a literal Nat head does not cover a rigid Nat (h7)" $
        isNoInstance (run (env [decl "Sz" [] [vec (NatLitU 3) int]]) [] (sz (vec (NatVarU (TV "k")) int)))
          @?= True
    , testCase "the instantiation binds the head's Nat variable" $
        instOf (run (env [decl "Sz" [] [ForallU a (vec n (VarU a))]]) [] (sz (vec (NatLitU 5) str)))
          @?= Just [NatLitU 5, str]
    , testCase "CLS-16-2 table" $
        ( keyOf (run nested [] (sz (vec (NatLitU 3) (vec (NatLitU 4) int))))
        , keyOf (run nested [] (sz (vec (NatLitU 3) (vec (NatLitU 3) int)))) )
          @?= (Just (key "Sz" [vec (NatLitU 3) (vec (NatLitU 4) int)]), Just (key "Sz" [vec (NatLitU 3) (vec (NatLitU 3) int)]))
    , testCase "CLS-16-3 table" $
        keyOf (run (env [decl "Sz" [] [ForallU a (ForallU b (pair (VarU a) (VarU b)))], decl "Sz" [] [ForallU c (pair (VarU c) int)]]) [] (sz (pair str int)))
          @?= Just (key "Sz" [ForallU c (pair (VarU c) int)])
    , testCase "incomparable maxima are ambiguous (E3)" $
        isAmbiguous (run (env [decl "Sz" [] [ForallU b (pair bool (VarU b))], decl "Sz" [] [ForallU a (pair (VarU a) str)]]) [] (sz (pair bool str)))
          @?= True
    , testCase "the nearest link of the alias chain decides" $
        ( keyOf (run (env [decl "Sz" [] [VarU name], decl "Sz" [] [str]]) [] (sz (VarU name)))
        , keyOf (run (env [decl "Sz" [] [str]]) [] (sz (VarU name))) )
          @?= (Just (key "Sz" [VarU name]), Just (key "Sz" [str]))
    , testCase "no coercion to an optional (CLS-17, E4)" $
        isNoInstance (run (env [decl "Sz" [] [str]]) [] (sz (OptionalU str))) @?= True
    , testCase "effect rows match only when equal (G7)" $
        ( keyOf (run (env [decl "Sz" [] [EffectU io int]]) [] (sz (EffectU io int)))
        , isNoInstance (run (env [decl "Sz" [] [EffectU io int]]) [] (sz (EffectU other int)))
        , isNoInstance (run (env [decl "Sz" [] [int]]) [] (sz (EffectU io int))) )
          @?= (Just (key "Sz" [EffectU io int]), True, True)
    , testCase "a legacy record is matched by its declared name (E12)" $
        keyOf (run (env [decl "Sz" [] [VarU foo]]) [] (sz (NamU NamRecord bar [] [(Key "n", str)])))
          @?= Just (key "Sz" [VarU foo])
    , testCase "a met context gives its evidence (N1)" $
        run withCtx [] (sz (w str))
          @?= Right (EvInstance (key "Sz" [ForallU a (w (VarU a))]) [str] [EvInstance (key "Sz" [str]) [] []])
    , testCase "an unmet context is an error (N1)" $
        isContextUnmet (run withCtx [] (sz (w bool))) @?= True
    , testCase "a context met by the signature (N2)" $
        ( run withCtx [sz (VarU a)] (sz (w (VarU a)))
        , isContextUnmet (run withCtx [] (sz (w (VarU a)))) )
          @?= (Right (EvInstance (key "Sz" [ForallU a (w (VarU a))]) [VarU a] [EvGiven (sz (VarU a))]), True)
    , testCase "a superclass of a given is given" $
        run (insertClassInfo (ClassName "Big") (ClassInfo [a] [sz (VarU a)] Map.empty) (env [])) [Constraint (ClassName "Big") [VarU a]] (sz (VarU a))
          @?= Right (EvGiven (sz (VarU a)))
    , testCase "a context that does not shrink stops (G5)" $
        isLoop (run (env [decl "Sz" [sz (w (VarU a))] [ForallU a (VarU a)]]) [] (sz int)) @?= True
    , testCase "a context variable the head lacks is undetermined" $
        ( case run (env [decl "Sz" [sz (VarU b)] [int]]) [] (sz int) of
            Left (ContextUnmet _ _ (Undetermined _ _)) -> True
            _ -> False )
          @?= True
    , testCase "an open class argument is undetermined under Q1 (a)" $
        isUndetermined (run (env [decl "Sz" [] [ForallU a (list (VarU a))]]) [] (sz (list ex))) @?= True
    , testCase "Q1 (b): one head at every instantiation" $
        ( keyOf (resolve skolem mempty (env [decl "Sz" [] [ForallU a (list (VarU a))]]) [] (sz (list ex)))
        , isUndetermined (resolve skolem mempty (env [decl "Sz" [] [ForallU a (list (VarU a))], decl "Sz" [] [list int]]) [] (sz (list ex)))
        , isUndetermined (resolve skolem mempty (env [decl "Sz" [] [ForallU a (VarU a)]]) [] (sz partial)) )
          @?= (Just (key "Sz" [ForallU a (list (VarU a))]), True, True)
    , testCase "Packable's wire argument under Q2 (a)" $
        let packable = (env [decl "Packable" [] [int, VarU name]]) {ieFundeps = packableWireFundep}
         in ( keyOf (run packable [] (pk ex (VarU name)))
            , isNoInstance (run packable [] (pk str (VarU name)))
            , isUndetermined (run (env [decl "Packable" [] [int, VarU name]]) [] (pk ex (VarU name))) )
              @?= (Just (key "Packable" [int, VarU name]), True, True)
    , testCase "DERIV-6 derives Eq where no instance matches, behind the policy flag" $
        ( run (env []) [] (eq (pair int str))
        , resolve derive mempty (env []) [] (eq (pair int str))
        , isNotCrossable (resolve derive mempty (env []) [] (eq (FunU [int] int))) )
          @?= ( Left (NoInstance (eq (pair int str)) [])
              , Right (EvDerived (ClassName "Eq") (pair int str))
              , True )
    , testCase "declarations of one head merge" $
        map (Map.keys . idMethods) (instanceDecls (env [declWith "Sz" [int] "m1", declWith "Sz" [int] "m2"]) (ClassName "Sz"))
          @?= [[EV "m1", EV "m2"]]
    , testCase "an omitted method falls through to a more general instance (CLS-12 (b))" $
        let e = env [declWith "Sz" [ForallU a (VarU a)] "m", decl "Sz" [] [int]]
         in case run e [] (sz int) of
              Left err -> assertFailure (show err)
              Right ev ->
                ( methodImpls defaultPolicy e ev (EV "m")
                , methodImpls defaultPolicy {policyOmitted = OmittedMissing} e ev (EV "m") )
                  @?= (Right [body "m"], Left (MethodOmitted (key "Sz" [int]) (EV "m")))
    ]
  where
    a = TV "a"
    b = TV "b"
    c = TV "c"
    n = NatVarU (TV "n")
    int = VarU (TV "Int")
    str = VarU (TV "Str")
    bool = VarU (TV "Bool")
    name = TV "Name"
    foo = TV "Foo"
    bar = TV "Bar"
    ex = ExistU (TV "e") ([], Open) ([], Open)
    partial = ExistU (TV "p") ([int, str], Open) ([], Open)
    io = EffectSet (Set.singleton "IO")
    other = EffectSet (Set.singleton "F")
    vec x y = AppU (VarU (TV "Vec")) [x, y]
    w x = AppU (VarU (TV "W")) [x]
    list x = AppU (VarU (TV "List")) [x]
    pair x y = AppU (VarU (TV "Tuple2")) [x, y]
    sz x = Constraint (ClassName "Sz") [x]
    eq x = Constraint (ClassName "Eq") [x]
    pk x y = Constraint (ClassName "Packable") [x, y]
    body m = ImplBody (ExprI 0 (VarE defaultValue (EV m)))
    decl cls ctx ts = mkInstanceDecl Declared (ClassName cls) ctx ts Map.empty 0
    declWith cls ts m = mkInstanceDecl Declared (ClassName cls) [] ts (Map.singleton (EV m) [body m]) 0
    key cls ts = idKey (decl cls [] ts)
    env ds = foldr insertInstanceDecl defaultValue (reverse ds)
    nat3 = env [decl "Sz" [] [ForallU a (vec n (VarU a))], decl "Sz" [] [vec (NatLitU 3) int]]
    nested = env [decl "Sz" [] [ForallU a (vec n (vec n (VarU a)))], decl "Sz" [] [vec (NatLitU 3) (vec (NatLitU 4) int)], decl "Sz" [] [vec (NatLitU 3) (vec (NatLitU 3) int)]]
    withCtx = env [decl "Sz" [] [str], decl "Sz" [sz (VarU a)] [ForallU a (w (VarU a))]]
    scope =
      Map.fromList
        [ (name, [([], str, ArgDocAlias defaultValue, False, TypedefAlias)])
        , (foo, [([], NamU NamRecord bar [] [(Key "n", str)], ArgDocAlias defaultValue, False, TypedefAlias)])
        ]
    run = resolve defaultPolicy scope
    skolem = defaultPolicy {policyOpenArgs = OpenArgsSkolemUnique}
    derive = defaultPolicy {policyDerive = True}
    keyOf (Right (EvInstance k _ _)) = Just k
    keyOf _ = Nothing
    instOf (Right (EvInstance _ ts _)) = Just ts
    instOf _ = Nothing
    isNoInstance (Left (NoInstance _ _)) = True
    isNoInstance _ = False
    isAmbiguous (Left (Ambiguous _ _)) = True
    isAmbiguous _ = False
    isContextUnmet (Left (ContextUnmet {})) = True
    isContextUnmet _ = False
    isLoop (Left (ContextLoop _)) = True
    isLoop _ = False
    isUndetermined (Left (Undetermined _ _)) = True
    isUndetermined _ = False
    isNotCrossable (Left (NotCrossable _)) = True
    isNotCrossable _ = False
