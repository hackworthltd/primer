module Tests.Shadowing where

import Foreword

import Hedgehog hiding (Property, check, property, withDiscards, withTests)
import Data.Data (Data)
import Primer.Core
import Primer.Name
import qualified Data.Tree as T
import qualified Data.Set as Set
import qualified Data.Generics.Uniplate.Data as U
import Primer.Zipper
import Optics
import Data.Tree.Optics (root)
import Primer.Builtins
import Primer.Primitives
import Primer.Gen.Core.Typed
import Primer.Core.Utils
import Tests.EvalFull
import Primer.EvalFull
import Primer.Typecheck
import Primer.Module
import Tests.Gen.Core.Typed (propertyWTInExtendedGlobalCxt)
import Primer.Core.Transform (unfoldApp)
import Primer.Core.DSL 
import Test.Tasty.HUnit hiding ((@?=))
import qualified Data.Map as Map
import Primer.Gen.Core.Raw (genName, genModuleName)
import Primer.Action.Available -- (actionsForDefBody, actionsForDef) -- TODO: make all imports explicit
import qualified Hedgehog.Gen as Gen
import Data.List.Extra (enumerate)
import Hedgehog.Internal.Property (forAllWithT)
import Primer.Action
import qualified Data.Text as T
import Primer.App (appProg, Prog (..), handleEditRequest, runEditAppM, EditAppM)
import qualified Primer.App as App
import Primer.Test.TestM (evalTestM)
import Tasty (Property)
import Primer.Def
import Primer.TypeDef
import Primer.Test.App (runAppTestM)
import Test.Tasty.HUnit ((@?=))
import Primer.Test.Util (vcn, tcn, gvn)


-- The 'a' parameter (node labels) are only needed for implementation of 'binderTree'
-- TODO: really? What for, why not binderTreeTy?
--   (currently using them to store expr-at-root for type tree)
--   nb: this is an edge-labeled tree: the edges show what binders scope over in that child
data Tree a b = Node a [(b,Tree a b)]
  deriving Show

noNodeLabels :: Tree a b -> Tree () b
noNodeLabels = bimap (const ()) identity

rootLabel :: Tree a b -> a
rootLabel (Node a _) = a

instance Bifunctor Tree where
  bimap f g (Node a xs) = Node (f a) $ map (bimap g (bimap f g)) xs

--drawTree :: Tree String String -> String
drawTree = T.drawTree . f
  where
    f (Node a xs) = T.Node a $ map (\(b,t) -> f t & root %~ (("[" <> b <> "]--") <>)) xs


foldTree :: (a -> [(b,c)] -> c) -> Tree a b -> c
foldTree f (Node a xs) = f a $ map (second $ foldTree f) xs

-- NB: there are no children in kinds, so we need not look in the metadata
-- NB: any binder in types (∀ only) scopes over all type children
binderTreeTy :: (Data b, Eq b) => Type' b -> Tree () (Set Name)
binderTreeTy = noNodeLabels . binderTreeTy'
 where
   -- we remember the Type in the node labels, so know which children binders scope over
   binderTreeTy' :: (Data b, Eq b) => Type' b -> Tree (Type' b) (Set Name)
   binderTreeTy' = U.para $ \ty children ->
          Node ty $ map (\c -> (Set.map unLocalName $ getBoundHereTy ty (Just $ rootLabel c), c)) children

-- Note this DOES NOT check if anything in the metadata's TypeCache is
-- shadowed (or is shadowing) Currently it happens that we can ascribe
-- a type of '∀a. _' to a subterm that happens to be under an 'a'
-- binder. See https://github.com/hackworthltd/primer/issues/556
noShadowing :: (Data a, Data b, Eq a, Eq b) => Expr' a b -> Shadowing
noShadowing = checkShadowing . binderTree

noShadowingTy :: (Data b, Eq b) => Type' b -> Shadowing
noShadowingTy = checkShadowing . binderTreeTy

binderTree :: forall a b. (Data a, Data b, Eq a, Eq b) => Expr' a b -> Tree () (Set Name)
binderTree = noNodeLabels . go
  where
    noNodeLabels' :: Tree () b' -> Tree (Maybe a') b'
    noNodeLabels' = bimap (const Nothing) identity
    go :: Expr' a b -> Tree (Maybe (Expr' a b)) (Set Name)
    go = U.para $ \e exprChildren' ->
      let exprChildren = map (\c ->  case rootLabel c of
                                                    Nothing -> (mempty, c) -- no term binders scope over metadata or type children
                                                    c'@(Just _) -> (getBoundHere e c', c)) --(getBoundHere e c',_) {-getBoundHereUp $ FA {prior = c'', current = e},c-}))
                         exprChildren'
          typeChildren = case target . focusOnlyType <$> focusType (focus e) of
            -- TODO: document why don't need to bind anything...
            Just ty -> [(mempty,binderTreeTy ty)]
            Nothing -> mempty
            {-
          metaChildren = case e ^. _exprMetaLens % _type of
            Nothing -> mempty
            Just (TCChkedAt ty) -> [binderTreeTy ty]
            Just (TCSynthed ty) -> [binderTreeTy ty]
            Just (TCEmb (TCBoth ty1 ty2)) -> [binderTreeTy ty1, binderTreeTy ty2]
-} -- don't include metadata. see #556
      in Node (Just e) $ exprChildren <> (noNodeLabels' <<$>> (typeChildren {- <> metaChildren-}))

data Shadowing = ShadowingExists | ShadowingNotExists
  deriving (Eq, Show)

checkShadowing :: Tree () (Set Name) -> Shadowing
checkShadowing t = if fst $ foldTree f t
  then ShadowingExists
  else ShadowingNotExists
  where
    f :: () -> [(Set Name,(Bool,Set Name))] -> (Bool,Set Name)
    f () xs = let allSubtreeBinds = Set.unions $ map (snd.snd) xs
                  bindsHere = Set.unions $ map fst xs
                  allBinds = bindsHere <> allSubtreeBinds
                  shadowing = any (\(bs, (s, bs')) -> s || not (Set.disjoint bs bs')) xs
              in (shadowing, allBinds)

-- Inlining a global can shadow
-- We simply need to alpha-convert first, but the term may be big, and we need to do a full substitution
-- NB: this is essentially the same problem as case-of-known-ctor!
unit_global_shadow :: Assertion
unit_global_shadow =
  let globalName = gvn ["M"] "x"
      ((def, expr, expected), maxID) = create $ do
        dt <- tcon tBool `tfun` tcon tBool
        de <- lam "y" $ lvar "y"
        let d = DefAST $ ASTDef {
                 astDefType = dt
                , astDefExpr = de}
        e <-  lam "y" $ gvar globalName
        expect <- lam "y" $ (lam "y" $ lvar "y") `ann` (tcon tBool `tfun` tcon tBool)
        pure (d,e, expect)
   in do
        s <- evalFullTest maxID mempty (Map.singleton globalName def) 1 Chk expr
        s <~==> Left (TimedOut expected)
        noShadowing expected @?= ShadowingNotExists

unit_known_case_shadow :: Assertion
unit_known_case_shadow =
  let ((expr, expected), maxID) = create $ do
        e <- lam "x" $ case_ ((con1' ["M"] "C" emptyHole) `ann` tcon' ["M"] "D")
                             [branch' (["M"],"C") [("t",Nothing)] emptyHole]
        expect <- lam "x" $ let_ "t" (emptyHole `ann` tforall "x" KType (tvar "x")) emptyHole
        pure (e, expect)
      td = TypeDefAST $ ASTTypeDef {
              astTypeDefParameters = mempty
              , astTypeDefConstructors = [ValCon (vcn ["M"] "C") [TForall () "x" KType $ TVar () "x"]]
              , astTypeDefNameHints = mempty}
   in do
        s <- evalFullTest maxID (Map.singleton (tcn ["M"] "D") td) mempty 1 Chk expr
        distinctIDs s
        s <~==> Left (TimedOut expected)
        noShadowing expected @?= ShadowingNotExists

-- 2 problems when bring info from typedef into prog:
-- - introduce a let-binding for parameter. This is in new scope and can shadow an outer binder
-- - introduce a type (as bound thing) which can have binders, with similar problems
-- TODO: don't really need the "s"; this test subsumes the one above
-- We would need to alpha-convert first, but the type may be big, and we need to do a full substitution
unit_known_case_shadow_2 :: Assertion
unit_known_case_shadow_2 =
  let ((expr, expected), maxID) = create $ do
        e <- lAM "x" $ case_ ((con' ["M"] "C" [emptyHole, emptyHole]) `ann` (tcon' ["M"] "D" `tapp` tvar "x"))
                             [branch' (["M"],"C") [("s",Nothing),("t",Nothing)] emptyHole]
        expect <- lAM "x" $ let_ "s" (emptyHole `ann` tcon tBool) $
           let_ "t" (emptyHole `ann` tlet "t" (tvar "x") (tforall "x" KType (tvar "t"))) emptyHole
        pure (e, expect)
      td = TypeDefAST $ ASTTypeDef {
              astTypeDefParameters = [("t", KType)]
              , astTypeDefConstructors = [ValCon (vcn ["M"] "C") [TCon () tBool,
                                                                  TForall () "x" KType $ TVar () "t"]]
              , astTypeDefNameHints = mempty}
   in do
        s <- evalFullTest maxID (Map.singleton (tcn ["M"] "D") td) mempty 1 Chk expr
        distinctIDs s -- TODO: do I really want to check this here...
        s <~==> Left (TimedOut expected)
        noShadowing expected @?= ShadowingNotExists

-- Consider @let x = λy._ in λy._@, when we push, we get @λy.let x = λy._ in _@, which shadows!
-- We simply need to alpha-convert first (which we already do if there would be capture)
-- but this will build up bigger substitutions, which seems problematic
unit_push_let_shadow :: Assertion
unit_push_let_shadow =
  let ((expr, expected), maxID) = create $ do
        e <- let_ "x" (lam "y" emptyHole) $ lam "y" $ lvar "x"
        expect <- lam "y" $ let_ "x" (lam "y" emptyHole) $ lvar "x"
        pure (e, expect)
   in do
        s <- evalFullTest maxID mempty mempty 1 Chk expr
        s <~==> Left (TimedOut expected)
        noShadowing expected @?= ShadowingNotExists

getEvalResultExpr :: Either EvalFullError Expr -> Expr
getEvalResultExpr = \case
  Left (TimedOut e) -> e
  Right e -> e
