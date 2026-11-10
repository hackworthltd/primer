{-# LANGUAGE OverloadedLabels #-}

-- | This module contains the zipper types @ExprZ@ and @TypeZ@, and functions for
--  operating on them.
module Primer.Zipper (
  ExprZ,
  TypeZip,
  TypeZ,
  KindZ,
  KindTZ,
  BindZ (..),
  focusBind,
  unfocusBind,
  focusLVarBind,
  focusTyVarBind,
  focusTyVarBindInType,
  LVarBindZ',
  LVarBindZ,
  TyVarBindZ',
  TyVarBindZ,
  TyVarInTypeBindZ',
  TyVarInTypeBindZ,
  TyBindZ',
  TyBindZ,
  TypeLoc' (..),
  TypeLoc,
  unfocusTypeLoc,
  focusOnlyTypeBind,
  IsZipper (asZipper),
  Loc,
  Loc' (..),
  BindLoc,
  BindLoc' (..),
  focusType,
  focusKind,
  focusLoc,
  unfocusType,
  unfocusKind,
  unfocusKindT,
  focusOnlyKind,
  focusOnlyType,
  focus,
  unfocus,
  target,
  _target,
  replace,
  focusOn,
  focusOnTy,
  focusOnKind,
  top,
  up,
  down,
  left,
  right,
  farthest,
  FoldAbove,
  current,
  prior,
  foldAbove,
  foldAboveTypeZ,
  unfocusExpr,
  unfocusLoc,
  bindersAbove,
  bindersBelow,
  LetBinding' (..),
  LetBinding,
  letBindingName,
  LetTypeBinding' (..),
  getBoundHere,
  getBoundHereUp,
  getBoundHereDn,
  bindersAboveTy,
  bindersAboveTypeZ,
  getBoundHereTy,
  getBoundHereUpTy,
  getBoundHereDnTy,
  bindersBelowTy,
  SomeNode (..),
  findNodeWithParent,
  findNodeInType,
  FoldAbove' (FA),
) where

import Foreword

import Data.Data (Data)
import Data.Generics.Uniplate.Data (universeBi)
import Data.Generics.Uniplate.Zipper (
  Zipper,
  fromZipper,
  zipper,
 )
import Data.Set qualified as S
import Optics (
  set,
  singular,
  unsafeFiltered,
  view,
  (%),
  (.~),
  (^?),
 )
import Optics.Lens (lens)
import Primer.Core (
  Expr,
  Expr' (Con),
  ExprMeta,
  HasID (..),
  ID,
  Kind,
  KindMeta,
  LVarBind,
  LVarBind',
  LocalName (unLocalName),
  TyVarBind,
  TyVarBind',
  Type,
  Type' (TForall),
  TypeMeta,
  bindName,
  getID,
  scopedChildrenOf,
  typesInExpr,
  _exprLVarBinds,
  _exprTyVarBind,
  _typeTyVarBind,
 )
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (Name)
import Primer.Zipper.Nested (
  BindZ (..),
  IsZipper (..),
  ZipNest (ZipNest),
  down,
  focus,
  focusBind,
  innerZipNest,
  left,
  mergeNest,
  replace,
  right,
  target,
  top,
  unfocusBind,
  unfocusNest,
  up,
  _target,
 )
import Primer.Zipper.Type (
  FoldAbove,
  FoldAbove' (..),
  KindTZ,
  KindTZ',
  LetTypeBinding' (LetTypeBind),
  TyBindZ,
  TyBindZ',
  TypeLoc,
  TypeLoc' (..),
  TypeZip,
  TypeZip',
  bindersAboveTy,
  bindersBelowTy,
  farthest,
  focusOnKind,
  focusOnTy,
  focusOnTy',
  foldAbove,
  getBoundHereDnTy,
  getBoundHereTy,
  getBoundHereUpTy,
  search,
  unfocusKindT,
  unfocusTypeLoc,
 )

type ExprZ' a b c = Zipper (Expr' a b c) (Expr' a b c)

-- | An ordinary zipper for 'Expr's
type ExprZ = ExprZ' ExprMeta TypeMeta KindMeta

-- | A zipper for 'Type's embedded in expressions.
-- For such types, we need a way
-- to navigate around them without losing our place in the wider expression.
-- This type contains a Zipper for a 'Type' and a function that will place the
-- unzippered type back into the wider expression zipper, keeping its place.
type TypeZ' a b c = ZipNest (ExprZ' a b c) (TypeZip' b c) (Type' b c)

type TypeZ = TypeZ' ExprMeta TypeMeta KindMeta

-- | A zipper for 'Kind's embedded in expressions (which will always be inside a 'Type').
type KindZ' a b c = ZipNest (ExprZ' a b c) (KindTZ' b c) (Type' b c)

type KindZ = KindZ' ExprMeta TypeMeta KindMeta

-- | A specific location in our AST.
-- This can either be in an expression, type, or binding.
data Loc' a b c
  = -- | An expression
    InExpr (ExprZ' a b c)
  | -- | A type
    InType (TypeZ' a b c)
  | -- | A kind
    InKind (KindZ' a b c)
  | -- | A binding
    InBind (BindLoc' a b c)
  deriving stock (Generic)

type Loc = Loc' ExprMeta TypeMeta KindMeta

instance (HasID a, HasID b, HasID c) => HasID (Loc' a b c) where
  _id = lens getter setter
    where
      getter = \case
        InExpr e -> view _id e
        InType l -> view _id l
        InKind k -> view _id k
        InBind l -> view _id l
      setter l i = case l of
        InExpr e -> InExpr $ set _id i e
        InType t -> InType $ set _id i t
        InKind k -> InKind $ set _id i k
        InBind t -> InBind $ set _id i t

-- | A zipper for bindings of term variables by a λ, let, letrec or case branch.
type LVarBindZ' a b c = BindZ (ExprZ' a b c) (LVarBind' a)

type LVarBindZ = LVarBindZ' ExprMeta TypeMeta KindMeta

-- | A zipper for bindings of type variables by a Λ or let type.
type TyVarBindZ' a b c = BindZ (ExprZ' a b c) (TyVarBind' b)

type TyVarBindZ = TyVarBindZ' ExprMeta TypeMeta KindMeta

-- | A zipper for bindings of type variables by a ∀ or type-level let, in types embedded in expressions.
type TyVarInTypeBindZ' a b c = BindZ (TypeZ' a b c) (TyVarBind' b)

type TyVarInTypeBindZ = TyVarInTypeBindZ' ExprMeta TypeMeta KindMeta

-- | A location of a binding.
data BindLoc' a b c
  = -- | A binding of a term variable by a λ, let, letrec or case branch
    BindLVar (LVarBindZ' a b c)
  | -- | A binding of a type variable by a Λ or let type
    BindTyVar (TyVarBindZ' a b c)
  | -- | A binding of a type variable by a ∀ or type-level let, in a type
    BindTyVarInType (TyVarInTypeBindZ' a b c)
  deriving stock (Generic)

type BindLoc = BindLoc' ExprMeta TypeMeta KindMeta

instance (HasID a, HasID b) => HasID (BindLoc' a b c) where
  _id = lens getter setter
    where
      getter = \case
        BindLVar z -> view _id z
        BindTyVar z -> view _id z
        BindTyVarInType z -> view _id z
      setter l i = case l of
        BindLVar z -> BindLVar $ set _id i z
        BindTyVar z -> BindTyVar $ set _id i z
        BindTyVarInType z -> BindTyVarInType $ set _id i z

-- | Switch from an 'Expr' zipper to a 'Type' zipper, focusing on the type in
-- the current target. This expects that the target is an @Ann@, @App@,
-- @Letrec@ or @LetType@ node (as those are the only ones that contain a
-- @Type@).
focusType :: (Data a, Data b, Data c) => ExprZ' a b c -> Maybe (TypeZ' a b c)
focusType z = case target z of
  Con{} -> Nothing
  _ -> do
    t <- z ^? singular l
    pure $ ZipNest (zipper t) $ \t' -> z & l .~ t'
  where
    l = _target % typesInExpr

-- | Switch from an 'Type' zipper to a 'Kind' zipper, focusing on the kind in
-- the current target. This expects that the target is an @TForall@ node
-- (as this is the only one that contain a @Kind@).
focusKind :: (Data b, Data c) => TypeZ' a b c -> Maybe (KindZ' a b c)
focusKind (ZipNest z f) = case target z of
  TForall m n k t ->
    pure $
      ZipNest
        ( ZipNest
            (focus k)
            $ \k' -> replace (TForall m n k' t) z
        )
        f
  -- pure $ ZipNest (zipper t) $ \t' -> z & l .~ t'
  _ -> Nothing

-- Note that in each of the following, the use of 'unsafeFiltered' is safe, since we never modify bindings' IDs.

-- | Focus on the binding of a term variable with the given ID, if it is introduced by the target of the zipper.
focusLVarBind :: (Data a, Data b, Data c, HasID a) => ID -> ExprZ' a b c -> Maybe (LVarBindZ' a b c)
focusLVarBind i = focusBind $ singular $ _exprLVarBinds % unsafeFiltered ((== i) . getID)

-- | Focus on the binding of a type variable with the given ID, if it is introduced by the target of the zipper.
focusTyVarBind :: (Data a, Data b, Data c, HasID b) => ID -> ExprZ' a b c -> Maybe (TyVarBindZ' a b c)
focusTyVarBind i = focusBind $ singular $ _exprTyVarBind % unsafeFiltered ((== i) . getID)

-- | As 'focusTyVarBind', but in a type inside an expression.
focusTyVarBindInType :: (Data b, Data c, HasID b) => ID -> TypeZ' a b c -> Maybe (TyVarInTypeBindZ' a b c)
focusTyVarBindInType i = focusBind $ singular $ _typeTyVarBind % unsafeFiltered ((== i) . getID)

-- | Switch from a 'Type' zipper back to an 'Expr' zipper.
unfocusType :: (Data b, Data c) => TypeZ' a b c -> ExprZ' a b c
unfocusType = unfocusNest

-- | Switch from a 'Kind'-in-'Type'-in-'Expr' zipper back to an 'Type'-in-'Expr' zipper.
unfocusKind :: Data c => KindZ' a b c -> TypeZ' a b c
unfocusKind = mergeNest

-- | Forget the surrounding expression context
focusOnlyType :: TypeZ' a b c -> TypeZip' b c
focusOnlyType = innerZipNest

-- | Forget the surrounding expression context of a binding in a type
focusOnlyTypeBind :: TyVarInTypeBindZ' a b c -> TyBindZ' b c
focusOnlyTypeBind (BindZ (ZipNest z f) b u) = BindZ z b $ \b' z' -> focusOnlyType $ u b' (ZipNest z' f)

-- | Forget the surrounding expression context
focusOnlyKind :: KindZ' a b c -> KindTZ' b c
focusOnlyKind = innerZipNest

-- | Convert an 'Expr' to a 'Loc' which focuses on the top of the expression.
focusLoc :: Expr -> Loc
focusLoc = InExpr . focus

-- | Convert an 'Expr' zipper to an 'Expr'
unfocusExpr :: ExprZ' a b c -> Expr' a b c
unfocusExpr = fromZipper

-- | Convert a 'Loc' to an 'ExprZ'.
-- If we're in a type or kind or case binding, we'll shift focus up to the nearest enclosing expression.
unfocusLoc :: Loc -> ExprZ
unfocusLoc (InExpr z) = z
unfocusLoc (InType z) = unfocusType z
unfocusLoc (InBind (BindLVar z)) = unfocusBind z
unfocusLoc (InBind (BindTyVar z)) = unfocusBind z
unfocusLoc (InBind (BindTyVarInType z)) = unfocusType $ unfocusBind z
unfocusLoc (InKind k) = unfocusType $ unfocusKind k

-- | Convert a 'Loc' to an 'Expr'.
-- This shifts focus right up to the top, so the result is the whole expression.
unfocus :: Loc -> Expr
unfocus = unfocusExpr . unfocusLoc

-- | Focus on the node with the given 'ID', if it exists in the expression
focusOn :: (Data a, Data b, Data c, HasID a, HasID b, HasID c) => ID -> Expr' a b c -> Maybe (Loc' a b c)
focusOn i = focusOn' i . focus

-- | Focus on the node with the given 'ID', if it exists in the focussed expression
focusOn' :: (Data a, Data b, Data c, HasID a, HasID b, HasID c) => ID -> ExprZ' a b c -> Maybe (Loc' a b c)
focusOn' i = fmap snd . search matchesID
  where
    matchesID z
      -- If the current target has the correct ID, return that
      | getID (target z) == i = Just $ InExpr z
      -- If the target binds a variable with the correct ID, return that.
      -- If the target has an embedded type, search the type for a match.
      | otherwise =
          let inBinds =
                (InBind . BindLVar <$> focusLVarBind i z)
                  <|> (InBind . BindTyVar <$> focusTyVarBind i z)
              inType = do
                ZipNest tz f <- focusType z
                focusOnTy' i tz <&> \case
                  TInType tz' -> InType $ ZipNest tz' f
                  TInKind kz -> InKind (ZipNest kz f)
                  TInBind (BindZ tz' b u) -> InBind $ BindTyVarInType $ BindZ (ZipNest tz' f) b $ \b' (ZipNest t f') -> ZipNest (u b' t) f'
           in inBinds <|> inType

-- Gets all binders that scope over the focussed subtree
bindersAbove :: ExprZ -> S.Set Name
bindersAbove = foldAbove getBoundHereUp

foldAboveTypeZ ::
  (Data a, Data b, Data c, Monoid m) =>
  (FoldAbove (Type' b c) -> m) ->
  (FoldAbove' (Type' b c) (Expr' a b c) -> m) ->
  (FoldAbove (Expr' a b c) -> m) ->
  TypeZ' a b c ->
  m
foldAboveTypeZ inTy border inExpr tz =
  let tyz = focusOnlyType tz
      wholeTy = fromZipper tyz
      exz = unfocusType tz
   in foldAbove inTy tyz
        <> border FA{prior = wholeTy, current = target exz}
        <> foldAbove inExpr exz

bindersAboveTypeZ :: TypeZ -> S.Set Name
bindersAboveTypeZ =
  foldAboveTypeZ
    (S.map unLocalName . getBoundHereUpTy)
    -- Since nothing both contains a type and binds a variable, we
    -- could write (const mempty) for the "border" argument,
    -- but let's keep it around as future proofing
    (\FA{current} -> getBoundHere current Nothing)
    getBoundHereUp

-- Get the names bound by this layer of an expression for a given child.
getBoundHereUp :: (Eq a, Eq b, Eq c) => FoldAbove (Expr' a b c) -> S.Set Name
getBoundHereUp e = getBoundHere (current e) (Just $ prior e)

-- | The names bound anywhere within the focussed subtree, including within types.
bindersBelow :: ExprZ -> S.Set Name
bindersBelow z =
  S.fromList $
    map (unLocalName . bindName) (universeBi @_ @LVarBind $ target z)
      <> map (unLocalName . bindName) (universeBi @_ @TyVarBind $ target z)

-- Get all names bound by this layer of an expression, for any child.
-- E.g. for a "match" we get all vars bound by each branch.
getBoundHereDn :: (Eq a, Eq b, Eq c) => Expr' a b c -> S.Set Name
getBoundHereDn e = getBoundHere e Nothing

-- Get the names bound by this layer of an expression (both term and type names)
-- The second arg is the child we just came out of, if traversing up (and thus
-- need to extract binders based on which case branch etc), and Nothing if
-- traversing down (and want to get all binders regardless of branch).
getBoundHere :: (Eq a, Eq b, Eq c) => Expr' a b c -> Maybe (Expr' a b c) -> S.Set Name
getBoundHere e prev =
  S.fromList
    [ either (unLocalName . bindName) (unLocalName . bindName) b
    | (c, bs) <- scopedChildrenOf e
    , maybe True ((== c) . Left) prev
    , b <- bs
    ]

data LetBinding' a b c
  = LetBind (LVarBind' a) (Expr' a b c)
  | LetrecBind (LVarBind' a) (Expr' a b c) (Type' b c)
  | LetTyBind (LetTypeBinding' b c)
  deriving stock (Eq, Show)
type LetBinding = LetBinding' ExprMeta TypeMeta KindMeta

letBindingName :: LetBinding' a b c -> Name
letBindingName = \case
  LetBind n _ -> unLocalName $ bindName n
  LetrecBind n _ _ -> unLocalName $ bindName n
  LetTyBind (LetTypeBind n _) -> unLocalName $ bindName n

-- | Find a node in the AST by its ID, and also return its parent
findNodeWithParent ::
  ID ->
  Expr ->
  Maybe (SomeNode, Maybe SomeNode)
findNodeWithParent id x = do
  z <- focusOn id x
  Just $ case z of
    InExpr ez -> (ExprNode $ target ez, ExprNode . target <$> up ez)
    InType tz ->
      ( TypeNode $ target tz
      , Just $
          maybe
            (ExprNode $ target $ unfocusType tz)
            (TypeNode . target)
            (up tz)
      )
    InKind kz ->
      ( KindNode (target kz)
      , Just $ maybe (TypeNode $ target $ unfocusKind kz) (KindNode . target) $ up kz
      )
    InBind (BindLVar bz) -> (LVarBindNode $ bindZFocus bz, Just . ExprNode . target . unfocusBind $ bz)
    InBind (BindTyVar bz) -> (TyVarBindNode $ bindZFocus bz, Just . ExprNode . target . unfocusBind $ bz)
    InBind (BindTyVarInType bz) -> (TyVarBindNode $ bindZFocus bz, Just . TypeNode . target . unfocusBind $ bz)

-- | Find a sub-type, kind or binding in a larger type by its ID.
findNodeInType :: ID -> Type -> Maybe SomeNode
findNodeInType id ty =
  focusOnTy id ty <&> \case
    TInType t -> TypeNode $ target t
    TInKind k -> KindNode $ target k
    TInBind b -> TyVarBindNode $ bindZFocus b

-- | An AST node tagged with its "sort" - i.e. if it's a type or expression or binding etc.
data SomeNode
  = ExprNode Expr
  | TypeNode Type
  | KindNode Kind
  | -- | A binding of a term variable, e.g. by a λ or in a case branch
    LVarBindNode LVarBind
  | -- | A binding of a type variable, e.g. by a Λ or ∀
    TyVarBindNode TyVarBind
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON SomeNode
