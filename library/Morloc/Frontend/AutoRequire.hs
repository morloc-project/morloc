{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Frontend.AutoRequire
Description : Fail fast on a discarded Try
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A fallible intrinsic returns a @Try@ rather than performing an effect that
may raise, which means a failure is now an ordinary value -- and a value
nobody looks at is a value nobody notices. The shape that makes this a
hazard is the one that reads most naturally:

> do
>   @save "results.json" y
>   report "done"

@\@save@'s result is bound to a discard variable, so without help the save
could fail and the block would carry on and print "done".

The rule this pass implements:

> A do-block statement auto-requires when nothing demands its value.

Concretely, a statement bound to a discard variable (see
'BT.doDiscardPrefix') whose type is headed by @Try@ gets a guard inserted
after it: the @Ok@ arm's payload if the tag matches, and otherwise a throw
carrying the @Err@ arm's message. Binding the result instead -- @r <- @save
p@ or @_ <- @save p@ -- is what opts out, because then something does
demand the value and what happens to it is the author's business.

The pass runs after 'Morloc.Frontend.Typecheck.resolveTypes', where every
type is a concrete 'Type' and instances are still 'Many', so a single
insertion covers every realization of the statement uniformly.
-}
module Morloc.Frontend.AutoRequire
  ( autoRequire
  ) where

import Morloc.Frontend.Namespace
import qualified Morloc.BaseTypes as BT
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE

-- | Insert a fail-fast guard after every discarded @Try@-typed statement.
autoRequire ::
  AnnoS (Indexed Type) Many Int -> MorlocMonad (AnnoS (Indexed Type) Many Int)
autoRequire = go
  where
    go :: AnnoS (Indexed Type) Many Int
       -> MorlocMonad (AnnoS (Indexed Type) Many Int)
    go (AnnoS g c (LetS v bound body)) = do
      bound' <- go bound
      body' <- go body
      mGuarded <- guardDiscarded v bound' body'
      return $ AnnoS g c (LetS v bound' (fromMaybe body' mGuarded))
    go (AnnoS g c e) = AnnoS g c <$> mapExprSM go e

    -- The guard, when this binding is a discarded Try. The result is the
    -- new let body: a second let whose bound expression is the require and
    -- whose body is the original continuation. Nothing consumes the
    -- require's value either -- its point is the throw on the Err arm.
    guardDiscarded ::
      EVar ->
      AnnoS (Indexed Type) Many Int ->
      AnnoS (Indexed Type) Many Int ->
      MorlocMonad (Maybe (AnnoS (Indexed Type) Many Int))
    guardDiscarded v bound@(AnnoS (Idx i _) c _) body
      | not (BT.doDiscardPrefix `MT.isPrefixOf` unEVar v) = return Nothing
      | otherwise = do
          t <- resolvedType i (typeSofAnnoS bound)
          case tryArms t of
            Nothing -> return Nothing
            Just (errT, okT) -> do
              let idx = Idx i
                  subject = AnnoS (idx t) c (BndS v)
                  nameOf n = AnnoS (idx (VarT BT.str)) c (StrS n)
                  fieldIdx = AnnoS (idx (VarT BT.int)) c (IntS i 0)
                  field n ft = AnnoS (idx ft) c
                    (IntrinsicS IntrCtorField [subject, nameOf n, fieldIdx])
                  cond = AnnoS (idx (VarT BT.bool)) c
                    (IntrinsicS IntrTagTest [subject, nameOf BT.tryOkCtor])
                  thrown = AnnoS (idx okT) c
                    (IntrinsicS IntrThrow [field BT.tryErrCtor errT])
                  guardE = AnnoS (idx okT) c
                    (IfS cond (field BT.tryOkCtor okT) thrown)
              guardVar <- freshGuardVar
              return . Just $ AnnoS (idx (typeSofAnnoS body)) c
                (LetS guardVar guardE body)

    typeSofAnnoS (AnnoS (Idx _ t) _ _) = t

    -- Expand aliases before inspecting the head, so a `type MyOutcome =
    -- Try Str A` fires too. An unresolvable type is left alone rather than
    -- rejected: it cannot be shown to be a Try, and failing the build on a
    -- type the evaluator cannot reduce would reject programs that compiled
    -- before this pass existed.
    resolvedType :: Int -> Type -> MorlocMonad Type
    resolvedType i t = do
      scope <- MM.getGeneralScope i
      return $ case TE.evaluateType scope (type2typeu t) of
        Right t' -> typeOf t'
        Left _ -> t

    -- The error and payload types of a Try, peeling any effect row.
    tryArms :: Type -> Maybe (Type, Type)
    tryArms (EffectT _ t) = tryArms t
    tryArms (AppT (VarT v) [errT, okT]) | v == BT.tryVar = Just (errT, okT)
    tryArms _ = Nothing

    freshGuardVar = do
      n <- MM.getCounter
      return (EV (BT.doDiscardPrefix <> "req_" <> MT.pack (show n)))
