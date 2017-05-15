module Morloc.NodeAttribute
(
    NodeAttr(..)
  , nodeAttrS
) where

data NodeAttr =
  NodeAttr {
      nodeId    :: Maybe Int
    , nodeValue :: Maybe String
    , nodeType  :: Maybe String
    , primitive  :: Bool
  }
  deriving(Show,Eq,Ord)

nodeAttrS :: String -> NodeAttr
nodeAttrS s = NodeAttr {
    nodeId    = Nothing
  , nodeValue = Just s
  , nodeType  = Nothing
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
