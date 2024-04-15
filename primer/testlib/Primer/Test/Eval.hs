module Primer.Test.Eval (
  noTermShadowing,
  noTypeShadowing,
  recursiveLetRec',
  annotatedConstructor,
  stuckTerm,
  workerMap,
  caseRedex,
  annotatedPair,
  letrecLambda,
  constructorEtaAbstraction,
  lambdaShadow,
  mapHole,
  holeAnnotateCase,
  unsaturatedPrimitive,
  primitiveAnnotation,
  lazyPrimitive1,
  lazyPrimitive2,
  primitivePartialMap,
) where

import Foreword

import Data.Map qualified as M
import Primer.Builtins (
  cFalse,
  cMakePair,
  cSucc,
  cTrue,
  cZero,
  tBool,
  tList,
  tNat,
  tPair,
 )
import Primer.Builtins.DSL (
  bool_,
  list_,
 )
import Primer.Core (
  Expr,
  GVarName,
  LVarName,
  ModuleName,
  TyVarName,
 )
import Primer.Core.DSL (
  S,
  aPP,
  ann,
  app,
  branch,
  case_,
  char,
  con,
  con',
  con0,
  con1,
  emptyHole,
  gvar,
  hole,
  ktype,
  lam,
  letType,
  let_,
  letrec,
  lvar,
  tEmptyHole,
  tapp,
  tcon,
  tcon',
  tforall,
  tfun,
  tvar,
 )
import Primer.Def (
  Def,
  DefMap,
 )
import Primer.Examples qualified as Examples (
  even,
  map,
  map',
  odd,
 )
import Primer.Primitives (
  PrimDef (
    PrimConst,
    ToUpper
  ),
  tChar,
 )
import Primer.Primitives.DSL (
  pfun,
 )
import Primer.Test.Util (
  primDefs,
 )

-- | Ensure we don't have shadowing issues with types.
--
-- Note that the new name of the renamed type variable is
-- implementation-dependent.
noTypeShadowing :: TyVarName -> S (Expr, Expr)
noTypeShadowing implTVarRename = do
  e <- letType "a" (tvar "b") $ emptyHole `ann` (tcon' ["M"] "T" `tapp` tvar "a" `tapp` tforall "a" ktype (tvar "a") `tapp` tforall "b" ktype (tcon' ["M"] "S" `tapp` tvar "a" `tapp` tvar "b"))
  let b' = implTVarRename
  expect <- emptyHole `ann` (tcon' ["M"] "T" `tapp` tvar "b" `tapp` tforall "a" ktype (tvar "a") `tapp` tforall b' ktype (tcon' ["M"] "S" `tapp` tvar "b" `tapp` tvar b'))
  pure (e, expect)

-- | Ensure we don't have shadowing issues with terms.
--
-- Note that the new name of the renamed term variable is
-- implementation-dependent.
noTermShadowing :: LVarName -> S (Expr, Expr)
noTermShadowing implLVarName = do
  e <- let_ "a" (lvar "b") $ con' ["M"] "C" [lvar "a", lam "a" (lvar "a"), lam "b" (con' ["M"] "D" [lvar "a", lvar "b"])]
  let b' = implLVarName
  expect <- con' ["M"] "C" [lvar "b", lam "a" (lvar "a"), lam b' (con' ["M"] "D" [lvar "b", lvar b'])]
  pure (e, expect)

-- | @letrec x : Bool = x in x@
recursiveLetRec' :: S Expr
recursiveLetRec' = letrec "x" (lvar "x") (tcon tBool) (lvar "x")

-- | @True : Bool@
annotatedConstructor :: S (Expr, Expr)
annotatedConstructor = do
  tr <- con0 cTrue
  an <- ann (pure tr) (tcon tBool)
  pure (an, tr)

-- | @((λ x . x $ x) : ?) $ (λ x. x $ x)@
--
-- TODO: do we want to expand
--   (λ x. t) : ?
-- to
--   (λ x. t) : ? -> ?
-- and thus have an infinite derivation for
--   ((λ x . x x) : ?) (λ x. x x)
-- Currently we don't, so this is a stuck term
stuckTerm :: S Expr
stuckTerm = do
  let l = lam "x" $ lvar "x" `app` lvar "x"
  (l `ann` tEmptyHole) `app` l

-- | A worker/wrapper'd @map@, presented in module-ish form.
--
-- Note that our step evaluator isn't effective on this wrapped @map@
-- due to its use of "push-down lets". See the note in the step
-- evaluator's tests for details.
workerMap :: ModuleName -> Int -> S ([(GVarName, Def)], Expr, Expr)
workerMap modName n = do
  (mapName, mapDef) <- Examples.map' modName
  (evenName, evenDef) <- Examples.even modName
  (oddName, oddDef) <- Examples.odd modName
  let lst = list_ $ take n $ iterate (con1 cSucc) (con0 cZero)
  expr <- gvar mapName `aPP` tcon tNat `aPP` tcon tBool `app` gvar evenName `app` lst
  let globs = [(mapName, mapDef), (evenName, evenDef), (oddName, oddDef)]
  expect <- list_ (take n $ cycle [con0 cTrue, con0 cFalse]) `ann` (tcon tList `tapp` tcon tBool)
  pure (globs, expr, expect)

-- | A case redex must have an scrutinee which is an annotated
-- constructor.
--
-- Plain constructors are not well-typed here, for bidirectionality
-- reasons, although they just fail to reduce rather than the
-- evaluator throwing a type error.
caseRedex :: S (Expr, Expr, Expr)
caseRedex = do
  annCase <-
    case_
      (con0 cZero `ann` tcon tNat)
      [ branch cZero [] $ con0 cTrue
      , branch cSucc [("n", Nothing)] $ con0 cFalse
      ]
  noannCase <-
    case_
      (con0 cZero)
      [ branch cZero [] $ con0 cTrue
      , branch cSucc [("n", Nothing)] $ con0 cFalse
      ]
  expect <- con0 cTrue
  pure (annCase, noannCase, expect)

-- | Fully-evaluated pairs should be anotated.
annotatedPair :: ModuleName -> S ([(GVarName, Def)], Expr, Expr)
annotatedPair modName = do
  (evenName, evenDef) <- Examples.even modName
  (oddName, oddDef) <- Examples.odd modName
  let ty = tcon tNat `tfun` (tcon tPair `tapp` tcon tBool `tapp` tcon tNat)
  let expr1 =
        let_ "x" (con0 cZero)
          $ lam "n" (con cMakePair [gvar evenName `app` lvar "n", lvar "x"])
          `ann` ty
  expr <- expr1 `app` con0 cZero
  let globs = [(evenName, evenDef), (oddName, oddDef)]
  expect <-
    con cMakePair [con0 cTrue, con0 cZero]
      `ann` (tcon tPair `tapp` tcon tBool `tapp` tcon tNat)
  pure (globs, expr, expect)

-- | Apply a lambda bound to a letrec.
letrecLambda :: S (Expr, Expr)
letrecLambda = do
  -- 'f' is a bit silly here, but could just as well be a definition of 'even'
  let f =
        lam "x"
          $ case_
            (lvar "x")
            [ branch cZero [] $ con0 cTrue
            , branch cSucc [("i", Nothing)] $ lvar "f" `app` lvar "i"
            ]
  expr <- let_ "n" (con0 cZero) $ letrec "f" f (tcon tNat `tfun` tcon tBool) $ lvar "f" `app` lvar "n"
  expect <- con0 cTrue `ann` tcon tBool
  pure (expr, expect)

-- | In Primer, value constructors must be fully saturated, so to use
-- them partially applied, they must be eta-abstracted (i.e., wrapped
-- in a lambda).
constructorEtaAbstraction :: S (Expr, Expr)
constructorEtaAbstraction = do
  expr <- (lam "x" (con' ["M"] "C" [lvar "x", let_ "x" (con0 cTrue) (lvar "x"), lvar "x"]) `ann` (tcon tNat `tfun` tcon tBool)) `app` con0 cZero
  expect <- con' ["M"] "C" [con0 cZero, con0 cTrue, con0 cZero] `ann` tcon tBool
  pure (expr, expect)

-- | Another shadowing test.
lambdaShadow :: S (Expr, Expr)
lambdaShadow = do
  expr <- (lam "x" (lam "x" $ lvar "x") `ann` (tcon tBool `tfun` (tcon tNat `tfun` tcon tNat))) `app` con0 cTrue `app` con0 cZero
  expect <- con0 cZero `ann` tcon tNat
  pure (expr, expect)

mapHole :: ModuleName -> S (DefMap, Expr, Expr)
mapHole modName = do
  (mapName, mapDef) <- Examples.map modName
  let n = 3
  let lst = list_ $ take n $ iterate (con1 cSucc) (con0 cZero)
  e <- gvar mapName `aPP` tcon tNat `aPP` tcon tBool `app` emptyHole `app` lst
  let globs = [(mapName, mapDef)]
  expect <- list_ (take n $ ((emptyHole `ann` (tcon tNat `tfun` tcon tBool)) `app`) <$> iterate (con1 cSucc) (con0 cZero)) `ann` (tcon tList `tapp` tcon tBool)
  pure (M.fromList globs, e, expect)

holeAnnotateCase :: S Expr
holeAnnotateCase = hole $ ann (case_ emptyHole []) (tcon tBool)

-- | Unsaturated primitives are stuck terms.
unsaturatedPrimitive :: S (Expr, DefMap)
unsaturatedPrimitive = (,) <$> pfun ToUpper <*> primDefs

primitiveAnnotation :: S (Expr, Expr, DefMap)
primitiveAnnotation =
  (,,)
    <$> ( pfun ToUpper
            `ann` (tcon tChar `tfun` tcon tChar)
        )
    `app` (char 'a' `ann` tcon tChar)
    <*> char 'A'
    <*> primDefs

lazyPrimitive1 :: S (Expr, Expr, DefMap)
lazyPrimitive1 =
  (,,)
    <$> pfun PrimConst
    `app` bool_ True
    `app` emptyHole
    <*> bool_ True
    `ann` tcon tBool
    <*> primDefs

lazyPrimitive2 :: S (Expr, Expr, DefMap)
lazyPrimitive2 =
  (,,)
    <$> pfun PrimConst
    `app` bool_ True
    `app` letrec "x" (lvar "x") (tcon tNat) (lvar "x")
    <*> bool_ True
    `ann` tcon tBool
    <*> primDefs

primitivePartialMap :: ModuleName -> S (Expr, Expr, DefMap, DefMap)
primitivePartialMap modName = do
  (mapName, mapDef) <- Examples.map' modName
  (,,,)
    <$> gvar mapName
    `aPP` tcon tChar
    `aPP` tcon tChar
    `app` pfun ToUpper
    `app` list_
      [ char 'a'
      , char 'b'
      , char 'c'
      ]
    <*> list_
      [ char 'A'
      , char 'B'
      , char 'C'
      ]
    `ann` (tcon tList `tapp` tcon tChar)
    <*> pure (M.singleton mapName mapDef)
    <*> primDefs
