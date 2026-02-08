# Parsing Algorithms

## Precedence Climbing

Morloc uses **precedence climbing** (Pratt parsing) for infix operators.

**Key Idea:** Parse recursively, accepting only operators with precedence ≥ minimum threshold.

## Implementation

**Entry Point** (Parser.hs:619-623):
```haskell
pInfixExpr :: Parser ExprI
pInfixExpr = pPrecedenceClimb 0  -- Start with minPrec = 0
```

**Algorithm** (Parser.hs:624-667):
```haskell
pPrecedenceClimb :: Int -> Parser ExprI
pPrecedenceClimb minPrec = do
  lhs <- try pApp <|> pAtom
  pClimb lhs minPrec
  where
    pClimb lhs minPrec' = do
      maybeOp <- optional $ try $ lookAhead $ do
        opName <- pInfixOperator
        s <- CMS.get
        let (assoc, prec) = lookupFixity opName s
        if prec < minPrec'
          then fail "precedence too low"
          else return (opName, assoc, prec)

      case maybeOp of
        Nothing -> return lhs
        Just (opName, assoc, prec) -> do
          _ <- pInfixOperator
          let nextMinPrec = case assoc of
                InfixL -> prec + 1  -- Right needs higher prec
                InfixR -> prec      -- Right can have same prec
                InfixN -> prec + 1
          rhs <- pPrecedenceClimb nextMinPrec
          appExpr <- exprI (AppE (VarE opName) [lhs, rhs])
          pClimb appExpr minPrec'
```

## Examples

**Left-Associative:**
```morloc
infixl 6 +
1 + 2 + 3
```
Parse: `(1 + 2) + 3`

**Right-Associative:**
```morloc
infixr 5 :
1 : 2 : []
```
Parse: `1 : (2 : [])`

**Mixed Precedence:**
```morloc
infixl 7 *
infixl 6 +
1 + 2 * 3
```
Parse: `1 + (2 * 3)`

## Desugaring

All infix expressions desugar to function application:
```morloc
a + b          →  AppE (VarE "+") [a, b]
a + b * c      →  AppE (VarE "+") [a, AppE (VarE "*") [b, c]]
```

## Fixity Lookup

Default fixity: `(InfixL, 9)`

Fixity table consulted during parsing:
```haskell
lookupFixity :: EVar -> ParserState -> (Associativity, Int)
lookupFixity opName s =
  Map.findWithDefault (InfixL, 9) opName (stateFixityTable s)
```

## Parser-Lexer Interaction

1. Parser encounters operator
2. Looks up fixity in `stateFixityTable`
3. Uses precedence/associativity to guide parsing
4. Builds application AST node

---
*See also: [[INFIX-OPERATORS.md]], [[FIXITY-BUGS.md]], [[FRONTEND.md]]*
