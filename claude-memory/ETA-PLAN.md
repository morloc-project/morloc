Here is Claude's plan:
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
 Typechecker Exponential Complexity Fix

 Part 1: Understanding the Type System

 1.1 Theoretical Foundation

 The Morloc typechecker implements Dunfield-style bidirectional type checking based on "Complete
 and Easy Bidirectional Typechecking for Higher-Rank Polymorphism" (Dunfield & Krishnaswami,
 2013). This is a well-studied algorithm with formal soundness and completeness proofs.

 1.2 Key Data Structures

 Gamma (Context) - An ordered list of type bindings:
 data GammaIndex
   = VarG TVar               -- Type variable marker (for scoping)
   = AnnG EVar TypeU         -- Term variable has type (x : A)
   | ExistG TVar ...         -- Unsolved existential (â)
   | SolvedG TVar TypeU      -- Solved existential (â = τ)
   | MarkG TVar              -- Scope marker for cutting (>â)

 The context is a stack - new bindings are added to the front. This ordering is critical for
 well-scopedness.

 TypeU - Types with existentials:
 data TypeU
   = VarU TVar                          -- Universal variable (a)
   | ExistU TVar (params) (records)     -- Existential variable (â)
   | ForallU TVar TypeU                 -- Universal quantifier (∀a. A)
   | FunU [TypeU] TypeU                 -- Function (A → B → C)
   | AppU TypeU [TypeU]                 -- Type application (F a b)
   | NamU ...                           -- Named/record types

 1.3 Bidirectional Checking

 Two judgment forms (Dunfield Figure 7):

 1. Synthesis (synthE): Γ ⊢ e ⇒ A ⊣ Δ
   - Given context Γ and expression e, output type A and updated context Δ
   - Used when type is unknown (e.g., primitives, variables)
 2. Checking (checkE): Γ ⊢ e ⇐ A ⊣ Δ
   - Given context Γ, expression e, and expected type A, output updated context Δ
   - Used when type is known from context (e.g., function arguments)

 Key connection - the Sub rule:
   Γ ⊢ e ⇒ A ⊣ Θ       Θ ⊢ [Θ]A <: [Θ]B ⊣ Δ
   ─────────────────────────────────────────── Sub
             Γ ⊢ e ⇐ B ⊣ Δ
 To check e against B: synthesize A, then verify A is a subtype of B.

 1.4 Subtyping Rules (Dunfield Figure 9)

 The subtype function implements:

            Γ ⊢ A <: A ⊣ Γ                              (<:Var)

   Γ ⊢ B₁ <: A₁ ⊣ Θ       Θ ⊢ [Θ]A₂ <: [Θ]B₂ ⊣ Δ
   ──────────────────────────────────────────────       (<:→)
           Γ ⊢ A₁→A₂ <: B₁→B₂ ⊣ Δ

   Γ,â ⊢ [â/a]A <: B ⊣ Δ,â,Θ
   ────────────────────────────                         (<:∀L)
       Γ ⊢ ∀a.A <: B ⊣ Δ

   Γ,a ⊢ A <: B ⊣ Δ,a,Θ
   ─────────────────────                                (<:∀R)
     Γ ⊢ A <: ∀a.B ⊣ Δ

       â ∉ FV(A)       Γ[â] ⊢ A ≤: â ⊣ Δ
   ─────────────────────────────────────────            (<:InstantiateL/R)
            Γ[â] ⊢ â <: A ⊣ Δ

 1.5 Instantiation Rules (Dunfield Figure 10)

 When an existential meets a concrete type, solve it:

   Γ[â],â₁,â₂,â=â₁→â₂ ⊢ A₁ ≤: â₁ ⊣ Θ       Θ ⊢ â₂ ≤: [Θ]A₂ ⊣ Δ
   ────────────────────────────────────────────────────────────── InstLArr
               Γ[â] ⊢ A₁→A₂ ≤: â ⊣ Δ

   (symmetric for InstRArr)

                     Γ[â] ⊢ τ
   ──────────────────────────────────                   InstLSolve/InstRSolve
      Γ[â],Θ ⊢ â ≤: τ ⊣ Γ[â=τ],Θ

 1.6 Context Application (Dunfield Figure 8)

 The apply function applies solved existentials:
 apply :: Gamma -> TypeU -> TypeU
 apply g (VarU v) = case lookupU v g of
     Just t' -> t'           -- Solved: return solution
     Nothing -> VarU v       -- Unsolved: keep as-is
 apply g (FunU ts t) = FunU (map (apply g) ts) (apply g t)
 apply g (ExistU v ...) = case lookupU v g of
     Just t' -> apply g t'   -- Solved: apply recursively
     Nothing -> ExistU v ... -- Unsolved: apply to components
 ...

 ---
 Part 2: The Exponential Problems

 2.1 Problem 1: Eta Expansion Re-inference

 Location: Frontend/Typecheck.hs:376-391

 What happens:
 synthE i g0 f@(LamS vs x) = do
   (_, bodyType, _) <- synthG g0 x     -- (1) Synthesize body type
   let n = nfargs bodyType
   if n > 0 then do
     (g1, f2) <- expand n g0 f         -- (2) Wrap in n more lambdas
     synthE' i g1 f2                   -- (3) RE-SYNTHESIZE entire expression

 Why exponential: Consider \x -> \y -> \z -> f where f : A → B → C → D:
 1. Synthesize innermost f, find it returns A → B → C → D
 2. Need 3 more params, so expand \z -> f to \z a b c -> f a b c
 3. Re-synthesize the expanded form, which again synthesizes f
 4. This recursively applies at each lambda level

 For depth n, this is O(2^n) because each synthesis spawns another full synthesis.

 2.2 Problem 2: Subtype Function Apply per Argument

 Location: Typecheck/Internal.hs:189-191

 Current implementation:
 subtype scope (FunU (a1 : rs1) a2) (FunU (b1 : rs2) b2) g1 = do
   g2 <- subtype scope b1 a1 g1                                    -- Step 1
   subtype scope (apply g2 (FunU rs1 a2)) (apply g2 (FunU rs2 b2)) g2  -- Step 2

 This directly implements the <:→ rule from Dunfield. The rule requires applying [Θ] to the
 continuation types because subtyping the first argument may solve existentials that appear in
 later arguments.

 Why O(n²): For A₁ → A₂ → ... → Aₙ → R:
 - Step 1: apply to (A₂ → ... → Aₙ → R) - walks n-1 elements
 - Step 2: apply to (A₃ → ... → Aₙ → R) - walks n-2 elements
 - ...
 - Step n-1: apply to (Aₙ → R) - walks 1 element

 Total: (n-1) + (n-2) + ... + 1 = O(n²)

 2.3 Problem 3: Instantiate with Fold + Apply

 Location: Typecheck/Internal.hs:314-325

 instantiate scope (ExistU v ([], _) _) (FunU as b) g1 = do
   ...
   g5 <- foldlM (\g (e, t) -> instantiate scope e t g) g4 (zip eas as)
   instantiate scope eb (apply g5 b) g5

 Creates fresh existentials for each argument, folds over them calling instantiate, then applies
 the accumulated context to the return type. Similar O(m*n) pattern.

 ---
 Part 3: Proposed Fixes with Correctness Justification

 3.1 Fix for Problem 2: Batch Subtype Arguments

 Proposed change to Internal.hs:188-191:

 -- BEFORE (per-argument apply):
 subtype scope (FunU (a1 : rs1) a2) (FunU (b1 : rs2) b2) g1 = do
   g2 <- subtype scope b1 a1 g1
   subtype scope (apply g2 (FunU rs1 a2)) (apply g2 (FunU rs2 b2)) g2

 -- AFTER (batch apply):
 subtype scope (FunU as1 ret1) (FunU as2 ret2) g0
   | length as1 /= length as2 = Left $ SubtypeError (FunU as1 ret1) (FunU as2 ret2) "arity
 mismatch"
   | null as1 = subtype scope ret1 ret2 g0  -- Base case: just compare returns
   | otherwise = do
       -- Process all arguments (contravariant), accumulating context
       g1 <- foldlM (\g (b, a) -> subtype scope b a g) g0 (zip as2 as1)
       -- Apply context once to return types, then subtype
       subtype scope (apply g1 ret1) (apply g1 ret2) g1

 Correctness Justification:

 The Dunfield <:→ rule is:
   Γ ⊢ B₁ <: A₁ ⊣ Θ       Θ ⊢ [Θ]A₂ <: [Θ]B₂ ⊣ Δ
   ──────────────────────────────────────────────
           Γ ⊢ A₁→A₂ <: B₁→B₂ ⊣ Δ

 For multi-argument functions A₁ → A₂ → ... → Aₙ → R, the intermediate applies are conservative -
 they ensure that if A₁ <: B₁ solves some existential â = τ, that solution is visible when
 checking A₂ <: B₂.

 Key insight: The apply operations are monotonic. Once â = τ is in gamma, later subtype calls can
 only add more solutions, not remove them. Therefore, we can defer all applies until the end
 without changing the final result.

 Proof sketch:
 1. Let g₀ be initial context, g_i be context after subtyping argument i
 2. Original: subtype(apply(g_i, A_{i+1}), apply(g_i, B_{i+1}), g_i)
 3. Proposed: subtype(A_{i+1}, B_{i+1}, g_i)

 The difference is whether we apply g_i before or after the final subtype. But lookupU in apply
 will find the same solutions in g_i whether we apply early or late. The final apply g_n before
 return types captures all solutions.

 Potential issue: If argument types share existentials with each other (not just with return
 type), early apply might affect later subtyping.

 Counter-argument: Morloc's FunU normalizes to flat argument lists. Arguments are processed
 left-to-right, and existentials in later arguments can only reference solutions from earlier
 arguments, which are already in gamma. The apply before comparing returns handles
 cross-references.

 Recommendation: This optimization is safe for the common case. Add a flag to fall back to
 per-argument apply if issues arise.

 3.2 Fix for Problem 1: Avoid Re-inference in Eta Expansion

 The core problem: synthE for LamS synthesizes the body, then if eta expansion is needed, throws
 away that work and re-synthesizes the entire expanded expression.

 Proposed approach: Synthesize body once, then construct the expanded form using the
 already-inferred body type.

 synthE i g0 f@(LamS vs x) = do
   -- Create existentials for bound variables
   let (g1, paramTypes) = statefulMap (\g v -> newvar (unEVar v <> "_x") g) g0 vs
       g2 = foldl (\g (v, t) -> g ++> AnnG v t) g1 (zip vs paramTypes)

   -- Synthesize body ONCE
   (g3, bodyType, bodyExpr) <- synthG g2 x

   -- Construct result based on body type
   let normalBody = normalizeType (apply g3 bodyType)
   case normalBody of
     FunU extraArgs retType -> do
       -- Body returns a function: construct eta-expanded form
       -- WITHOUT re-synthesizing - reuse bodyExpr
       (g4, newVars, varExprs) <- createEtaVars g3 extraArgs
       appIdx <- MM.getCounter
       let fullArgs = map (apply g4) paramTypes ++ extraArgs
           expandedBody = makeApp bodyExpr varExprs appIdx
       return (g4, apply g4 (FunU fullArgs retType),
               LamS (vs ++ newVars) expandedBody)
     _ -> do
       -- Body is not a function: just return the lambda
       let funType = FunU (map (apply g3) paramTypes) bodyType
       return (g3, apply g3 funType, LamS vs bodyExpr)

 Correctness Justification:

 1. Type preservation: The body type is already inferred. If bodyType = FunU extraArgs retType,
 then wrapping in additional lambdas and applying to fresh variables preserves the type. The new
 lambda \x1...xn a1...am -> body a1...am has type (paramTypes ++ extraArgs) → retType.
 2. No lost information: The old approach:
   - Infer body type to get FunU extraArgs retType
   - Expand expression syntactically
   - Re-infer, which would find the same body type again

 The new approach skips the redundant re-inference since we already have bodyExpr with its type.
 3. Context consistency: We add AnnG v t for each new eta variable, maintaining well-scopedness.
 The final apply ensures all solved existentials are propagated.

 Key invariant: synthG g x returns (g', t, e') where e' is x annotated with type t at each
 subexpression. We can reuse this annotated e' when constructing the expanded form.

 3.3 Fix for Problem 3: Batch Instantiate

 Similar to fix 3.1 - the fold over instantiate is necessary, but the final apply g5 b can be done
  once. The current code already does this correctly; just verify no redundant applies inside the
 fold.

 ---
 Part 4: Implementation Plan

 Phase 1: Subtype Optimization (Low Risk)

 File: library/Morloc/Typecheck/Internal.hs

 1. Replace lines 188-191 with batched version
 2. Keep original as fallback behind flag if needed
 3. Test: Run stack test, ensure all typecheck tests pass

 Phase 2: Eta Expansion Fix (Medium Risk)

 File: library/Morloc/Frontend/Typecheck.hs

 1. Modify synthE for LamS pattern (lines 376-396)
 2. Add helper createEtaVars and makeApp
 3. Modify etaExpandSynthE to avoid re-synthesis when possible
 4. Test: Run full test suite, add complexity regression tests

 Phase 3: Verification

 Add to test-suite/UnitTypeTests.hs:
 -- Timeout: 5 seconds. Old code would timeout.
 testDeepComposition = localOption (mkTimeout 5000000) $
   assertGeneralType "deep composition"
     [r| compose f g x = f (g x)
         id x = x
         test = compose (compose (compose (compose id id) id) id) id
         test |]
     (forallu ["a"] (fun [var "a", var "a"]))

 ---
 Part 5: Files to Modify
 ┌──────────────────────────────────────┬─────────┬─────────────────────────────────┐
 │                 File                 │  Lines  │             Changes             │
 ├──────────────────────────────────────┼─────────┼─────────────────────────────────┤
 │ library/Morloc/Typecheck/Internal.hs │ 188-191 │ Batch subtype for FunU          │
 ├──────────────────────────────────────┼─────────┼─────────────────────────────────┤
 │ library/Morloc/Frontend/Typecheck.hs │ 376-396 │ Eta expansion without re-synth  │
 ├──────────────────────────────────────┼─────────┼─────────────────────────────────┤
 │ library/Morloc/Frontend/Typecheck.hs │ 516-552 │ Modify etaExpandSynthE          │
 ├──────────────────────────────────────┼─────────┼─────────────────────────────────┤
 │ test-suite/UnitTypeTests.hs          │ new     │ Add complexity regression tests │
 └──────────────────────────────────────┴─────────┴─────────────────────────────────┘
 ---
 Part 6: Risk Assessment
 Fix: Subtype batch
 Confidence: High
 Reasoning: Monotonicity of context ensures correctness
 ────────────────────────────────────────
 Fix: Eta expansion
 Confidence: Medium
 Reasoning: Reusing body type is sound but requires careful expr construction
 Mitigation: Implement Phase 1 first (isolated, low risk). Full test suite validation before Phase
  2. Add regression tests for complex type scenarios.
