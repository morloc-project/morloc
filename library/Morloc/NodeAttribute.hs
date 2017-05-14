module Morloc.NodeAttribute
(
    NodeAttr(..)
  , nodeAttrS
) where

data NodeAttr =
  NodeAttr {
      node_id    :: Maybe Int
    , node_value :: Maybe String
    , node_type  :: Maybe String
    , primitive  :: Bool
  }
  deriving(Show,Eq,Ord)

nodeAttrS :: String -> NodeAttr
nodeAttrS s = NodeAttr {
    node_id    = Nothing
  , node_value = Just s
  , node_type  = Nothing
  , primitive  = False
}

{- main :: IO ()                                      -}
{- main = do                                          -}
{-   let x = Function (Just 1) (Just "foo") (Nothing) -}
{-   print x                                          -}
{-   print $ valStr x                                 -}
{-   print $ typeStr x                                -}
{-   let a = Primitive (Float 1.2321)                 -}
{-   print a                                          -}
{-   print $ valStr a                                 -}
{-   print $ typeStr a                                -}
{-   let b = Primitive (String "asfd")                -}
{-   print b                                          -}
{-   print $ valStr b                                 -}
{-   print $ typeStr b                                -}
