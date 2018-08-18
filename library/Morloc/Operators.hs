{-|
Module      : Morloc.Operators
Description : Custom binary operators
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Operators
(
  -- * General purpose operators
    (|>>)
  , (<>)
  -- * RDF operators (inspired from those defined in HSparql)
  , (.:.)
  , (.^^.)
) where

import Data.Monoid
import qualified Data.RDF as DR
import qualified Data.Text as DT

-- | pipe the lhs functor into the rhs function
infixl 1 |>>
(|>>) :: Functor f => f a -> (a -> b) -> f b
(|>>) = flip fmap

-- | Join an RDF prefix and base creating a URI node
infix 9 .:.
(.:.) :: DT.Text -> DT.Text -> DR.Node
prefix .:. base = DR.UNode (prefix <> base)

-- | Sets an RDF datatype. (.^^.) has a lower precedence thatn (.:.), so the
-- two can be chainted as so:
--
-- > "45" .^^. "xsd" .:. "integer"
--
-- WARNING: This function fails if the rhs is a constructor other than UNode
infix 8 .^^.
(.^^.)
  :: DT.Text -- ^ The value
  -> DR.Node -- ^ An Data.RDF Node object of the UNode constructor, use of
             --   the other constructors causes instant death. 
  -> DR.Node -- ^ A Data.RDF.Node object of the LNode constructor
value .^^. (DR.UNode rdftype) = DR.LNode (DR.TypedL value rdftype)
_ = error "Can only set a type with a UNode"
