{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Primer.Eval.NormalOrder (
  RedexWithContext (RExpr, RType),
  findRedex,
  foldMapExpr,
  FMExpr (..),
  NormalOrderOptions (..),
  -- Exported for testing
  singletonCxt,
) where

import Foreword hiding (hoistAccum)

import Control.Monad.Log (MonadLog, WithSeverity)
import Control.Monad.Trans.Maybe (MaybeT)
import Optics (lens, (.~), (^.))
import Primer.Core (
  Expr,
  Expr' (
    APP,
    App,
    Case,
    Hole,
    LAM,
    Lam,
    Let,
    LetType,
    Letrec
  ),
  HasID (_id),
  Type' (
    TForall,
    TLet
  ),
 )
import Primer.Def (
  DefMap,
 )
import Primer.Eval.Redex (
  Cxt (..),
  Dir (..),
  EvalLog,
  Redex,
  RedexType,
  ViewRedexOptions,
  cxtAddLet,
  viewRedex,
  viewRedexType,
 )
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
import Primer.Log (ConvertLogMessage)
import Primer.TypeDef (
  TypeDefMap,
 )
import Primer.Zipper (
  ExprZ,
  IsZipper,
  LetBinding,
  LetBinding' (LetBind, LetTyBind, LetrecBind),
  TypeZ,
  down,
  focus,
  focusType,
  right,
  target,
 )
import Primer.Zipper.Type (
  LetTypeBinding' (LetTypeBind),
 )

-- We don't really want a zipper here, but a one-hole context, but it is easier
-- to just reuse the zipper machinery and ignore the target of the zipper.
-- The ExprZ/TypeZ is a zipper focussed on a subterm which is a Redex as described by the second argument.
data RedexWithContext
  = RExpr ExprZ Redex
  | RType TypeZ RedexType

-- TODO isn't there a simpler way to define this? it's a bit boilerplatey
instance HasID RedexWithContext where
  _id =
    lens
      ( \case
          RExpr a _ -> a ^. _id
          RType a _ -> a ^. _id
      )
      ( \case
          RExpr a b -> \a' -> RExpr (a & _id .~ a') b
          RType a c -> \a' -> RType (a & _id .~ a') c
      )

data ViewLet = ViewLet
  { bindingVL :: LetBinding
  -- ^ the binding itself
  , bodyVL :: (Dir, ExprZ)
  -- ^ the body (i.e. after the `in`)
  , typeChildrenVL :: [TypeZ]
  -- ^ any non-body type children
  , termChildrenVL :: [(Dir, ExprZ)]
  -- ^ any non-body term children (i.e. rhs of the binding)
  }
viewLet :: (Dir, ExprZ) -> Maybe ViewLet
viewLet dez@(_, ez) = case (target ez, exprChildren UnderBinders dez) of
  (Let _ x e _b, [ez', bz]) -> Just (ViewLet (LetBind x e) bz [] [ez'])
  (Letrec _ x e ty _b, [ez', bz]) -> Just (ViewLet (LetrecBind x e ty) bz tz [ez'])
  (LetType _ a ty _b, [bz]) -> bz `seq` Just (ViewLet (LetTyBind $ LetTypeBind a ty) bz tz [])
  _ -> Nothing
  where
    tz :: [TypeZ]
    tz = maybeToList $ focusType ez

data NormalOrderOptions
  = UnderBinders
  | StopAtBinders
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON NormalOrderOptions

-- | This is similar to 'foldMap', with a few differences:
-- - 'Expr' is not foldable
-- - We target every subexpression and also every (nested) subtype (e.g. in an annotation)
-- - We keep track of context and directionality (necessitating an extra 'Dir' argument, for "what directional context this whole expression is in")
--   (the "context" is just of the immediately-enclosing lets, which are the only ones that may "cross a binder")
-- - We handle @let@s specially, since we need to handle them differently when finding the normal order redex.
--   (When we hit the body of a @let@ (of any flavor), we use the provided 'subst' or 'substTy' argument, if provided, and do no further recursion.
--   If the corresponding argument is 'Nothing', then we recurse as normal.)
-- - We accumulate in some 'f a' for 'MonadPlus f' rather than an arbitrary monoid
--
-- The fold progresses in something similar to normal order, (modulo the special handling for @let@s).
-- Sepcifically, we find the root-most redex, and recurse into type children first, followed by term children left-to-right.
-- Since stuck terms (ignoring lets, because they have special handling) are stuck because of
-- their type annotation, or their first child, this will unblock progress at the root before doing too much work on its children.
-- However, we may reduce type children to normal form more eagerly than necessary,
-- both reducing type annotations more than needed, and reducing type applications when not needed.
-- Since computation in types is strongly normalising, this will not cause us to fail to find any normal forms.
--
-- We can optionally stop when we find a binder (e.g. to implement closed
-- evaluation -- do not compute under binders), although for consistency we
-- treat all case branches as being binders, even those that do not actually
-- bind a variable. Note that (for the case where we reduce a stack of @let@s
-- one-by-one inside-out) we need to go under let bindings, but stop when we
-- find a non-@let@.
foldMapExpr :: forall f a. MonadPlus f => NormalOrderOptions -> FMExpr (f a) -> Dir -> Expr -> f a
foldMapExpr opts extract topDir = go mempty . (topDir,) . focus
  where
    go :: Cxt -> (Dir, ExprZ) -> f a
    go lets dez@(d, ez) =
      extract.expr ez d lets
        <|> case viewLet dez of
          -- Prefer to compute inside the body of a let, but otherwise compute in the binding
          -- NB: we never push lets into lets, so the Cxt is reset for non-body children
          Just (ViewLet{bindingVL, bodyVL, typeChildrenVL, termChildrenVL}) ->
            msum $
              go (cxtAddLet bindingVL lets) bodyVL
                : map (goType mempty) typeChildrenVL
                  <> map (go mempty) termChildrenVL
          -- Since stuck things other than lets are stuck on the first child or
          -- its type annotation, we can handle them all uniformly
          -- Since this node is not a let, the context is reset
          _ ->
            case (opts, lets) of
              (StopAtBinders, Cxt (_ : _)) -> mzero
              _ ->
                msum $
                  (goType mempty =<< focusType' ez) -- NB: no binders in term scope over a type child
                    : map (go mempty) (exprChildren opts dez)
    goType :: Cxt -> TypeZ -> f a
    goType lets tz =
      extract.ty tz lets
        <|> case target tz of
          TLet _ a t _body
            -- Prefer to compute inside the body of a let, but otherwise compute in the binding
            | [tz', bz] <- typeChildren UnderBinders tz -> goType (cxtAddLet (LetTyBind $ LetTypeBind a t) lets) bz <|> goType mempty tz'
          _ -> msum $ map (goType mempty) $ typeChildren opts tz

data FMExpr m = FMExpr
  { expr :: ExprZ -> Dir -> Cxt -> m
  , ty :: TypeZ -> Cxt -> m
  }

focusType' :: MonadPlus m => ExprZ -> m TypeZ
focusType' = maybe empty pure . focusType

-- We find the normal-order redex.
findRedex ::
  forall l m.
  (MonadLog (WithSeverity l) m, ConvertLogMessage EvalLog l) =>
  NormalOrderOptions ->
  ViewRedexOptions ->
  TypeDefMap ->
  DefMap ->
  Dir ->
  Expr ->
  MaybeT m RedexWithContext
findRedex optsN optsV tydefs globals =
  foldMapExpr
    optsN
    ( FMExpr
        { expr = \ez d -> runReaderT (RExpr ez <$> viewRedex optsV tydefs globals d (target ez))
        , ty = \tz -> hoistMaybe . runReader (RType tz <<$>> viewRedexType optsV (target tz))
        }
    )

children' :: IsZipper za a => za -> [za]
children' z = case down z of
  Nothing -> mempty
  Just z' -> z' : unfoldr (fmap (\x -> (x, x)) . right) z'

exprChildren' :: (Dir, ExprZ) -> [(Dir, ExprZ)]
exprChildren' (d, ez) =
  children' ez <&> \c -> do
    let d' = case target ez of
          App _ f _ | f == target c -> Syn
          APP _ f _ | f == target c -> Syn
          Case _ scrut _ _ | scrut == target c -> Syn
          Hole _ _ -> Syn
          -- bodies of lets are the same direction as
          -- the let themselves
          Let _ _ e _
            | e == target c -> Syn
            | otherwise -> d
          LetType{} -> d
          Letrec _ _ e _ _
            | e == target c -> Chk
            | otherwise -> d
          _ -> Chk
    (d', c)

-- Extract the children of the current focus, except those under an binder.
-- This is used to restrict our evaluation to "closed evaluation".
-- NB: for consistency we skip all case branches, not just those that bind a variable.
exprChildrenClosed :: (Dir, ExprZ) -> [(Dir, ExprZ)]
exprChildrenClosed (d, ez) = case target ez of
  Lam{} -> []
  LAM{} -> []
  Let{} -> take 1 $ exprChildren' (d, ez) -- just the binding
  LetType{} -> []
  Letrec{} -> []
  Case{} -> take 1 $ exprChildren' (d, ez) -- just the scrutinee
  _ -> exprChildren' (d, ez)

exprChildren :: NormalOrderOptions -> (Dir, ExprZ) -> [(Dir, ExprZ)]
exprChildren = \case
  UnderBinders -> exprChildren'
  StopAtBinders -> exprChildrenClosed

typeChildren :: NormalOrderOptions -> TypeZ -> [TypeZ]
typeChildren = \case
  UnderBinders -> children'
  StopAtBinders -> \tz -> case target tz of
    TForall{} -> []
    _ -> children' tz

singletonCxt :: LetBinding -> Cxt
singletonCxt l = Cxt [l]
