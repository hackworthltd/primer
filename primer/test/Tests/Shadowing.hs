{-# LANGUAGE ViewPatterns #-}

module Tests.Shadowing where

import Foreword

import Data.Data (Data)
import Data.Generics.Uniplate.Data qualified as U
import Data.List.Extra (enumerate)
import Data.Set qualified as Set
import Hedgehog (annotateShow, discard, failure, forAll, label, success, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Primer.Action.Available (Action (NoInput), NoInputAction (LetToRec))
import Primer.App (
  App,
  appProg,
  handleEditRequest,
  progDefMap,
  progTypeDefMap,
  runEditAppM,
 )
import Primer.Core (
  Expr,
  Expr' (Case, Var),
  LocalName (unLocalName),
  TmVarRef (GlobalVarRef),
  Type',
 )
import Primer.Core.DSL (ann, app, create, create', lam, let_, lvar, tEmptyHole, tfun)
import Primer.Core.Utils (freeGlobalVars)
import Primer.Def (Def, astDefExpr, defAST, defType)
import Primer.Eval (
  AvoidShadowing (AvoidShadowing),
  Dir (Syn),
  NormalOrderOptions (StopAtBinders, UnderBinders),
  RunRedexOptions (RunRedexOptions, pushAndElide),
  ViewRedexOptions (ViewRedexOptions, aggressiveElision, avoidShadowing, groupedLets),
  redexes,
  step,
 )
import Primer.EvalFullStep (EvalFullError (..), EvalLog, evalFullStepCount)
import Primer.Gen.App (genApp)
import Primer.Gen.Core.Typed (forAllT, propertyWT)
import Primer.Log (runPureLog)
import Primer.Module (builtinModule, moduleDefsQualified, primitiveModule)
import Primer.Name (Name)
import Primer.Test.Util (failWhenSevereLogs)
import Primer.TypeDef (ASTTypeDef (..), TypeDef, typeDefAST, typeDefParameters, valConArgs)
import Primer.Typecheck (Cxt (typeDefs), SmartHoles (SmartHoles))
import Primer.Zipper (
  FoldAbove' (FA, current, prior),
  Loc' (InExpr),
  bindersAbove,
  focus,
  focusOn,
  focusOnlyType,
  focusType,
  getBoundHereUp,
  getBoundHereUpTy,
  target,
 )
import Tasty (Property, withDiscards, withTests)
import Test.Tasty.HUnit (Assertion)
import Tests.Action.Available (PA (..), genAction, toProgAction)
import Tests.Eval.Utils (genDirTm, testModules)
import Tests.EvalFullStep (evalFullTestAvoidShadowing, (<~==>))
import Tests.Gen.Core.Typed (propertyWTInExtendedGlobalCxt)

{-
Evaluation can result in variable shadowing even when there are no repeated binders in the initial term.
We detect this shadowing and rename to avoid it.
  (λf. f f : (? -> ?) -> ?) (λg y. g y)
reduces in 4 steps to
  (let g = (let f = λg y. g y : ? -> ? in f : ?) in λy. g y) : ? : ?
if we did not worry about shadowing, we would then push the 'let g' and get
  (λy. let g = (let f = λg y. g y : ? -> ? in f : ?) in g y) : ? : ?
where the 'y' is shadowed.
However, we instead do a renaming step and get
  (let g = (let f = λg y. g y : ? -> ? in f : ?) in λz. let y = z in g y) : ? : ?
-}
unit_shadow_no_reused_binders :: Assertion
unit_shadow_no_reused_binders =
  let ((e, expect), maxID) = create $ do
        e0 <-
          ( lam "f" (lvar "f" `app` lvar "f")
              `ann` ((tEmptyHole `tfun` tEmptyHole) `tfun` tEmptyHole)
            )
            `app` lam "g" (lam "y" $ lvar "g" `app` lvar "y")
        e4 <-
          ( let_
              "g"
              ( let_
                  "f"
                  (lam "g" (lam "y" $ lvar "g" `app` lvar "y") `ann` (tEmptyHole `tfun` tEmptyHole))
                  (lvar "f")
                  `ann` tEmptyHole
              )
              (lam "y" $ lvar "g" `app` lvar "y")
              `ann` tEmptyHole
            )
            `ann` tEmptyHole
        let z = "a88"
        e5 <-
          ( let_
              "g"
              ( let_
                  "f"
                  (lam "g" (lam "y" $ lvar "g" `app` lvar "y") `ann` (tEmptyHole `tfun` tEmptyHole))
                  (lvar "f")
                  `ann` tEmptyHole
              )
              (lam z $ let_ "y" (lvar z) $ lvar "g" `app` lvar "y")
              `ann` tEmptyHole
            )
            `ann` tEmptyHole
        pure (e0, [(0, e0), (4, e4), (5, e5)])
   in do
        for_ @[] expect $ \(s, ex) -> do
          a <- evalFullTestAvoidShadowing maxID mempty mempty s Syn e
          a <~==> Left (TimedOut ex)

-- We use this type to compute a "tree of binders", which are a view of
-- an expression (or type) where we only care about the tree shape and
-- what subtrees binders scope over. (In fact, we don't strictly preserve
-- the tree shape, as it is sometimes easier to insert extra empty nodes.)
-- The 'a' parameter (node labels) are only needed for implementation of
-- 'binderTree'
data Tree a b = Node a [(b, Tree a b)]
  deriving stock (Functor, Show)

instance Bifunctor Tree where
  bimap f g (Node a xs) = Node (f a) $ map (bimap g (bimap f g)) xs

foldTree :: (a -> [(b, c)] -> c) -> Tree a b -> c
foldTree f (Node a xs) = f a $ map (second $ foldTree f) xs

-- NB: there are no children in kinds, so we need not look in the metadata
-- NB: any binder in types (∀ only) scopes over all type children
binderTreeTy :: forall b c. (Data b, Data c, Eq b, Eq c) => Type' b c -> Tree () (Set Name)
binderTreeTy = noNodeLabels' . go
  where
    go :: Type' b c -> Tree (Type' b c) (Set Name)
    go = U.para $ \ty children ->
      Node ty $ map (\c@(Node c' _) -> (Set.map unLocalName $ getBoundHereUpTy FA{prior = c', current = ty}, c)) children

-- Note this DOES NOT check if anything in the metadata's TypeCache
-- is shadowed (or is shadowing); (see 'binderTree'). Currently
-- it happens that we can ascribe a type of '∀a. _' to
-- a subterm that happens to be under an 'a' binder. See
-- https://github.com/hackworthltd/primer/issues/556
noShadowing :: (Data a, Data b, Data c, Eq a, Eq b, Eq c) => Expr' a b c -> Shadowing
noShadowing = checkShadowing . binderTree

noShadowingTy :: (Data b, Data c, Eq b, Eq c) => Type' b c -> Shadowing
noShadowingTy = checkShadowing . binderTreeTy

noNodeLabels' :: Tree a' b' -> Tree () b'
noNodeLabels' = bimap (const ()) identity

binderTree :: forall a b c. (Data a, Data b, Data c, Eq a, Eq b, Eq c) => Expr' a b c -> Tree () (Set Name)
binderTree = noNodeLabels' . go
  where
    noNodeLabels :: Tree () b' -> Tree (Maybe a') b'
    noNodeLabels = bimap (const Nothing) identity
    go :: Expr' a b c -> Tree (Maybe (Expr' a b c)) (Set Name)
    go = U.para $ \e exprChildren' ->
      let exprChildren =
            map
              ( \c@(Node c' _) ->
                  ( case c' of
                      Nothing -> mempty -- no term binders scope over metadata or type children
                      Just c'' -> getBoundHereUp $ FA{prior = c'', current = e}
                  , c
                  )
              )
              exprChildren'
          typeChildren = case target . focusOnlyType <$> focusType (focus e) of
            Just ty -> [binderTreeTy ty]
            Nothing -> mempty
       in {-
             -- We don't include metadata. see
             -- https://github.com/hackworthltd/primer/issues/556
             metaChildren = case e ^. _exprMetaLens % _type of
               Nothing -> mempty
               Just (TCChkedAt ty) -> [binderTreeTy ty]
               Just (TCSynthed ty) -> [binderTreeTy ty]
               Just (TCEmb (TCBoth ty1 ty2)) -> [binderTreeTy ty1, binderTreeTy ty2]
             -}

          Node (Just e) $ exprChildren <> map ((mempty,) . noNodeLabels) typeChildren {- <> metaChildren-}

data Shadowing = ShadowingExists (Set Name) | ShadowingNotExists
  deriving stock (Eq, Show)

checkShadowing :: Tree () (Set Name) -> Shadowing
checkShadowing t =
  let shadows = fst $ foldTree f t
   in if Set.null shadows
        then ShadowingNotExists
        else ShadowingExists shadows
  where
    f :: () -> [(Set Name, (Set Name, Set Name))] -> (Set Name, Set Name)
    f () xs =
      let allSubtreeBinds = Set.unions $ map (snd . snd) xs
          bindsHere = Set.unions $ map fst xs
          allBinds = bindsHere <> allSubtreeBinds
          shadowing = Set.unions $ map (\(newBinders, (oldShadows, oldBinders)) -> oldShadows <> Set.intersection newBinders oldBinders) xs
       in (shadowing, allBinds)

-- Check evaluation does not introduce shadowing, except in some known cases
tasty_eval_full_shadow :: Property
tasty_eval_full_shadow = withTests 500
  $ withDiscards 2000
  $ propertyWTInExtendedGlobalCxt testModules
  $ do
    let optsV = ViewRedexOptions{groupedLets = True, aggressiveElision = True, avoidShadowing = True}
    let optsR = RunRedexOptions{pushAndElide = True}
    let globs = foldMap' moduleDefsQualified $ create' $ sequence testModules
    optsN <- forAll $ Gen.element @[] [StopAtBinders, UnderBinders]
    tds <- asks typeDefs
    (dir, t, _ty) <- genDirTm
    unless (noShadowing t == ShadowingNotExists) discard
    when (isKnownShadow optsN t) discard
    n <- forAll $ Gen.integral (Range.linear 1 20)
    (_steps, s) <- failWhenSevereLogs $ evalFullStepCount @EvalLog optsN optsV optsR tds globs n dir t
    annotateShow s
    noShadowing (getEvalResultExpr s) === ShadowingNotExists
  where
    -- Inlining a global under a binder may cause shadowing, as we don't track
    -- what new binders this may bring into scope
    -- This can be a global term definition, or a case reduction bringing in part of a global type defininiton
    isKnownShadow UnderBinders e = not $ Set.null (freeGlobalVars e) && null [() | Case{} <- U.universe e]
    isKnownShadow StopAtBinders _ = False

tasty_eval_shadow :: Property
tasty_eval_shadow =
  withDiscards 2000
    $ propertyWTInExtendedGlobalCxt testModules
    $ do
      let globs = foldMap' moduleDefsQualified $ create' $ sequence testModules
      tds <- asks typeDefs
      (dir, t, _ty) <- genDirTm
      unless (noShadowing t == ShadowingNotExists) discard
      rs <- failWhenSevereLogs $ redexes @EvalLog AvoidShadowing tds globs dir t
      when (null rs) discard
      i <- forAllT $ Gen.element rs
      when (isKnownShadow $ focusOn i t) discard
      s <- failWhenSevereLogs $ step @EvalLog AvoidShadowing tds globs t dir i
      case s of
        Left err -> annotateShow err >> failure
        Right (s', _) -> do
          annotateShow s
          noShadowing s' === ShadowingNotExists
  where
    -- Inlining a global under a binder may cause shadowing, as we don't track
    -- what new binders this may bring into scope
    isKnownShadow (Just (InExpr ez@(target -> Var _ (GlobalVarRef _)))) = not $ Set.null $ bindersAbove ez
    isKnownShadow _ = False

getEvalResultExpr :: Either EvalFullError Expr -> Expr
getEvalResultExpr = \case
  Left (TimedOut e) -> e
  Right e -> e

tasty_available_actions_shadow :: Property
tasty_available_actions_shadow = withDiscards 2000
  $ propertyWT []
  $ do
    l <- forAllT $ Gen.element enumerate
    cxt <- forAllT $ Gen.choice $ map sequence [[], [builtinModule], [builtinModule, primitiveModule]]
    a <- forAllT $ genApp SmartHoles cxt
    unless (all ((== ShadowingNotExists) . snd) $ noShadowingApp a) discard
    (def', loc, act) <- forAllT $ genAction l a
    let (defName, def) = (bimap fst fst def', bimap snd snd def')
    annotateShow defName
    annotateShow def
    annotateShow loc
    annotateShow act
    progActs' <- toProgAction l a (def, loc, act)
    progActs <- case progActs' of
      NoOpt progActs -> pure progActs
      OptOffered _ progActs -> pure progActs
      OptGen _ _ -> discard
      NoOfferedOpts -> discard
    let (res, _logs) = runPureLog $ runEditAppM (handleEditRequest progActs) a
    let isKnownShadow = case act of
          Just (NoInput LetToRec) ->
            {- The following action can introduce shadowing
            let bar = (let bar = ? in ?) in ?
            ~>
            letrec bar = (let bar = ? in ?) : ? in ?
            -}
            True
          _ -> False
    case (res, isKnownShadow) of
      ((Left err, _), _) -> annotateShow err >> failure
      ((Right _, _), True) -> label "known shadowing case" >> success
      ((Right _, a'), False) -> do
        label "not known shadowing case"
        annotateShow a'
        for_ (noShadowingApp a') $ \(d, s) -> do
          unless (s == ShadowingNotExists) $ annotateShow d
          s === ShadowingNotExists

noShadowingApp :: App -> [(Either (TypeDef () ()) Def, Shadowing)]
noShadowingApp a =
  let p = appProg a
      tds = toList (progTypeDefMap p)
      ds = toList (progDefMap p)
   in map (\d -> (Left d, noShadowingTypeDef d)) tds <> map (\d -> (Right d, noShadowingDef d)) ds
  where
    combineShadow ShadowingNotExists s = s
    combineShadow s ShadowingNotExists = s
    combineShadow (ShadowingExists x) (ShadowingExists y) = ShadowingExists $ x <> y
    noShadowingTypeDef = checkShadowing . binderTreeTypeDef
    noShadowingDef d =
      let sty = (noShadowingTy $ defType d)
          stm = (noShadowing . astDefExpr <$> defAST d)
       in sty & maybe identity (flip combineShadow) stm

binderTreeTypeDef :: (Data a, Data b, Eq a, Eq b) => TypeDef a b -> Tree () (Set Name)
binderTreeTypeDef ty =
  Node
    ()
    [
      ( Set.fromList $ unLocalName . fst <$> typeDefParameters ty
      , Node () $ (mempty,) . bindersVC <$> (astTypeDefConstructors =<< toList (typeDefAST ty))
      )
    ]
  where
    bindersVC vc = Node () $ valConArgs vc <&> (mempty,) . binderTreeTy
