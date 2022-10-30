{-# LANGUAGE OverloadedLabels #-}

-- | This module contains the zipper types @ExprZ@ and @TypeZ@, and functions for
--  operating on them.
module Primer.Zipper (
  ExprZ,
  TypeZip,
  TypeZ,
  CaseBindZ,
  updateCaseBind,
  unfocusCaseBind,
  caseBindZFocus,
  IsZipper (asZipper),
  Loc,
  Loc' (..),
  BindLoc,
  BindLoc' (..),
  focusType,
  focusLoc,
  unfocusType,
  focusOnlyType,
  focus,
  unfocus,
  target,
  _target,
  replace,
  focusOn,
  focusOnTy,
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
  foldBelow,
  unfocusExpr,
  unfocusLoc,
  locToEither,
  bindersAbove,
  bindersBelow,
  LetBinding' (..),
  LetBinding,
  letBindingName,
  LetTypeBinding' (..),
  getBoundHere',
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
  findType,
) where

import Foreword

import Data.Data (Data)
import Data.Generics.Product (position)
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Zipper (
  Zipper,
  fromZipper,
  zipper,
 )
import Data.List as List (delete)
import Data.Set qualified as S
import Optics (
  AffineTraversal',
  Field3 (_3),
  filteredBy,
  ifolded,
  iheadOf,
  iso,
  ix,
  only,
  preview,
  set,
  view,
  (%),
  (.~),
  (<%),
  (<%>),
  (^?),
 )
import Optics.Lens (Lens', lens)
import Primer.Core (
  Bind,
  Bind' (..),
  CaseBranch' (CaseBranch),
  Expr,
  Expr' (Case, LAM, Lam, Let, LetType, Letrec),
  ExprMeta,
  HasID (..),
  ID,
  LVarName,
  LocalName (unLocalName),
  Type,
  Type' (),
  TypeMeta,
  bindName,
  getID,
  typesInExpr,
 )
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (Name)
import Primer.Zipper.Type (
  FoldAbove,
  FoldAbove' (..),
  IsZipper (..),
  LetTypeBinding' (LetTypeBind),
  TypeZip,
  bindersAboveTy,
  bindersBelowTy,
  down,
  farthest,
  focus,
  focusOnTy,
  foldAbove,
  foldBelow,
  getBoundHereDnTy,
  getBoundHereTy,
  getBoundHereUpTy,
  left,
  replace,
  right,
  search,
  target,
  top,
  up,
  _target,
 )

type ExprZ' a b = Zipper (Expr' a b) (Expr' a b)

-- | An ordinary zipper for 'Expr's
type ExprZ = ExprZ' ExprMeta TypeMeta

type TypeZip' b = Zipper (Type' b) (Type' b)

-- | A zipper for 'Type's embedded in expressions.
-- For such types, we need a way
-- to navigate around them without losing our place in the wider expression.
-- This type contains a Zipper for a 'Type' and a function that will place the
-- unzippered type back into the wider expression zipper, keeping its place.
data TypeZ' a b = TypeZ (TypeZip' b) (Type' b -> ExprZ' a b)
  deriving (Generic)

type TypeZ = TypeZ' ExprMeta TypeMeta

instance HasID b => HasID (TypeZ' a b) where
  _id = position @1 % _id

-- | A zipper for variable bindings in case branches.
-- This type focuses on a particular binding in a particular branch.
-- It contains the focused binding, along with the the parent expression (as a zipper) and some
-- parts of the surrounding branch which are useful when renaming.
-- It also contains a function which can update the binding and the RHS of the case branch
-- simultaneously, yielding a new expression.
-- These fields are chosen to be convenient for renaming, and they may not be that useful for future
-- actions we want to perform.
data CaseBindZ' a b = CaseBindZ
  { caseBindZExpr :: ExprZ' a b
  -- ^ a zipper focused on the case expression
  , caseBindZFocus :: Bind' a
  -- ^ the focused binding
  , caseBindZRhs :: Expr' a b
  -- ^ the rhs of the branch
  , caseBindAllBindings :: [Bind' a]
  -- ^ all other bindings in the case branch, i.e. all except the focused one
  , caseBindZUpdate :: Bind' a -> Expr' a b -> ExprZ' a b -> ExprZ' a b
  -- ^ a function to update the focused binding and rhs simultaneously
  }
  deriving (Generic)

type CaseBindZ = CaseBindZ' ExprMeta TypeMeta

-- Apply an update function to the focus of a case binding, optionally modifying the rhs of the branch too.
-- The update function is given three arguments:
-- - the focused binding
-- - a list of all other bindings in the branch (not including the focused one)
-- - the rhs of the branch
-- It returns a tuple of the updated binding and the updated rhs.
-- This is very specialised to be useful when renaming case branch bindings.
-- It may not be very reusable but I think it's helpful to keep the complexity of 'CaseBindZ'
-- restricted to this module.
updateCaseBind ::
  Functor f =>
  CaseBindZ ->
  (Bind' ExprMeta -> [Bind' ExprMeta] -> Expr -> f (Bind' ExprMeta, Expr)) ->
  f CaseBindZ
updateCaseBind (CaseBindZ z bind rhs bindings update) f =
  f bind bindings rhs <&> \(bind', rhs') ->
    let z' = update bind' rhs' z
     in CaseBindZ z' bind' rhs' bindings update

instance HasID a => HasID (CaseBindZ' a b) where
  _id = #caseBindZFocus % _id

-- | A specific location in our AST.
-- This can either be in an expression, type, or binding.
data Loc' a b
  = -- | An expression
    InExpr (ExprZ' a b)
  | -- | A type
    InType (TypeZ' a b)
  | -- | A binding (currently just case bindings)
    InBind (BindLoc' a b)
  deriving (Generic)

type Loc = Loc' ExprMeta TypeMeta

instance (HasID a, HasID b) => HasID (Loc' a b) where
  _id = lens getter setter
    where
      getter = \case
        InExpr e -> view _id e
        InType l -> view _id l
        InBind l -> view _id l
      setter l i = case l of
        InExpr e -> InExpr $ set _id i e
        InType t -> InType $ set _id i t
        InBind t -> InBind $ set _id i t

-- | A location of a binding.
-- This only covers bindings in case branches for now.

{- HLINT ignore BindLoc' "Use newtype instead of data" -}
data BindLoc' a b
  = BindCase (CaseBindZ' a b)
  deriving (Generic)

type BindLoc = BindLoc' ExprMeta TypeMeta

instance HasID a => HasID (BindLoc' a b) where
  _id = position @1 % _id

-- | Switch from an 'Expr' zipper to a 'Type' zipper, focusing on the type in
-- the current target. This expects that the target is an @Ann@, @App@,
-- @Letrec@ or @LetType@ node (as those are the only ones that contain a
-- @Type@).
focusType :: (Data a, Data b) => ExprZ' a b -> Maybe (TypeZ' a b)
focusType z = do
  t <- z ^? l
  pure $ TypeZ (zipper t) $ \t' -> z & l .~ t'
  where
    l = _target % typesInExpr

-- | If the currently focused expression is a case expression, search the bindings of its branches
-- to find one matching the given ID, and return the 'Loc' for that binding.
-- If no match is found, return @Nothing@.
findInCaseBinds :: forall a b. (Data a, Data b, Eq a, HasID a) => ID -> ExprZ' a b -> Maybe (Loc' a b)
findInCaseBinds i z = do
  branches <- preview branchesLens z
  ((branchIx, bindIx), bind) <- branches & iheadOf (ifolded % binds <%> ifolded <% filteredBy (_id % only i))
  let branchLens = branchesLens % ix branchIx
  let rhsLens = branchLens % branchRHS
  rhs <- preview rhsLens z
  allBinds <- preview (branchLens % binds) z
  let bindLens = branchLens % binds % ix bindIx
  let update bind' rhs' = set rhsLens rhs' . set bindLens bind'
  pure $ InBind $ BindCase $ CaseBindZ z bind rhs (delete bind allBinds) update
  where
    branchesLens :: AffineTraversal' (ExprZ' a b) [CaseBranch' a b]
    branchesLens = _target % #_Case % _3
    binds :: Lens' (CaseBranch' a b) [Bind' a]
    binds = position @2
    branchRHS :: Lens' (CaseBranch' a b) (Expr' a b)
    branchRHS = position @3

-- | Switch from a 'Type' zipper back to an 'Expr' zipper.
unfocusType :: TypeZ' a b -> ExprZ' a b
unfocusType (TypeZ zt f) = f (fromZipper zt)

-- | Forget the surrounding expression context
focusOnlyType :: TypeZ' a b -> TypeZip' b
focusOnlyType (TypeZ zt _) = zt

instance Data b => IsZipper (TypeZ' a b) (Type' b) where
  asZipper = position @1

-- 'CaseBindZ' is sort of a fake zipper which can only focus on one thing: the case binding.
-- It's a bit fiddly to make it appear as a zipper like this, but it's convenient to have a
-- consistent interface for 'ExprZ', 'TypeZ' and 'CaseBindZ'.
instance IsZipper CaseBindZ (Bind' ExprMeta) where
  asZipper = #caseBindZFocus % iso zipper fromZipper

-- | Convert an 'Expr' to a 'Loc' which focuses on the top of the expression.
focusLoc :: Expr -> Loc
focusLoc = InExpr . focus

-- Convert a 'CaseBindZ' to an 'ExprZ' by shifting focus to the parent case expression.
unfocusCaseBind :: CaseBindZ' a b -> ExprZ' a b
unfocusCaseBind = caseBindZExpr

-- | Convert an 'Expr' zipper to an 'Expr'
unfocusExpr :: ExprZ' a b -> Expr' a b
unfocusExpr = fromZipper

-- | Convert a 'Loc' to an 'ExprZ'.
-- If we're in a type or case binding, we'll shift focus up to the nearest enclosing expression.
unfocusLoc :: Loc -> ExprZ
unfocusLoc (InExpr z) = z
unfocusLoc (InType z) = unfocusType z
unfocusLoc (InBind (BindCase z)) = unfocusCaseBind z

-- | Convert a 'Loc' to 'Either ExprZ TypeZ'.
-- If the 'Loc' is in a case bind, we shift focus to the parent case expression.
-- This function is mainly to keep compatibility with code which still expects 'Either ExprZ TypeZ'
-- as a representation of an AST location.
locToEither :: Loc' a b -> Either (ExprZ' a b) (TypeZ' a b)
locToEither (InBind (BindCase z)) = Left $ unfocusCaseBind z
locToEither (InExpr z) = Left z
locToEither (InType z) = Right z

-- | Convert a 'Loc' to an 'Expr'.
-- This shifts focus right up to the top, so the result is the whole expression.
unfocus :: Loc -> Expr
unfocus = unfocusExpr . unfocusLoc

-- | Focus on the node with the given 'ID', if it exists in the expression
focusOn :: (Data a, Data b, Eq a, HasID a, HasID b) => ID -> Expr' a b -> Maybe (Loc' a b)
focusOn i = focusOn' i . focus

-- | Focus on the node with the given 'ID', if it exists in the focussed expression
focusOn' :: (Data a, Data b, Eq a, HasID a, HasID b) => ID -> ExprZ' a b -> Maybe (Loc' a b)
focusOn' i = fmap snd . search matchesID
  where
    matchesID z
      -- If the current target has the correct ID, return that
      | getID (target z) == i = Just $ InExpr z
      -- If the target has an embedded type, search the type for a match.
      -- If the target is a case expression with bindings, search each binding for a match.
      | otherwise =
          let inType = focusType z >>= search (guarded (== i) . getID . target) <&> fst <&> InType
              inCaseBinds = findInCaseBinds i z
           in inType <|> inCaseBinds

-- Gets all binders that scope over the focussed subtree
bindersAbove :: ExprZ -> S.Set Name
bindersAbove = foldAbove getBoundHereUp

foldAboveTypeZ ::
  (Data a, Data b, Monoid m) =>
  (FoldAbove (Type' b) -> m) ->
  (FoldAbove' (Type' b) (Expr' a b) -> m) ->
  (FoldAbove (Expr' a b) -> m) ->
  TypeZ' a b ->
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
getBoundHereUp :: (Eq a, Eq b) => FoldAbove (Expr' a b) -> S.Set Name
getBoundHereUp e = getBoundHere (current e) (Just $ prior e)

-- Get the names bound within the focussed subtree
bindersBelow :: ExprZ -> S.Set Name
bindersBelow = foldBelow getBoundHereDn

-- Get all names bound by this layer of an expression, for any child.
-- E.g. for a "match" we get all vars bound by each branch.
getBoundHereDn :: (Eq a, Eq b) => Expr' a b -> S.Set Name
getBoundHereDn e = getBoundHere e Nothing

-- Get the names bound by this layer of an expression (both term and type names)
-- The second arg is the child we just came out of, if traversing up (and thus
-- need to extract binders based on which case branch etc), and Nothing if
-- traversing down (and want to get all binders regardless of branch).
getBoundHere :: (Eq a, Eq b) => Expr' a b -> Maybe (Expr' a b) -> S.Set Name
getBoundHere e prev = S.fromList $ either identity letBindingName <$> getBoundHere' e prev

data LetBinding' a b
  = LetBind LVarName (Expr' a b)
  | LetrecBind LVarName (Expr' a b) (Type' b)
  | LetTyBind (LetTypeBinding' b)
type LetBinding = LetBinding' ExprMeta TypeMeta

letBindingName :: LetBinding' a b -> Name
letBindingName = \case
  LetBind n _ -> unLocalName n
  LetrecBind n _ _ -> unLocalName n
  LetTyBind (LetTypeBind n _) -> unLocalName n

getBoundHere' :: (Eq a, Eq b) => Expr' a b -> Maybe (Expr' a b) -> [Either Name (LetBinding' a b)]
getBoundHere' e prev = case e of
  Lam _ v _ -> anon v
  LAM _ tv _ -> anon tv
  Let _ v rhs b ->
    if maybe True (== b) prev
      then letBind $ LetBind v rhs
      else mempty
  Letrec _ v rhs t _ -> letBind $ LetrecBind v rhs t
  LetType _ v t _ -> letBind $ LetTyBind $ LetTypeBind v t
  Case _ _ bs ->
    let binderss = map (\(CaseBranch _ ns rhs) -> (rhs, map (unLocalName . bindName) ns)) bs
     in case prev of
          Nothing -> concatMap (fmap Left . snd) binderss
          Just p -> concatMap (\(b, binders) -> if b == p then Left <$> binders else mempty) binderss
  _ -> mempty
  where
    anon x = [Left $ unLocalName x]
    letBind l = [Right l]

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
    InBind (BindCase bz) -> (CaseBindNode $ caseBindZFocus bz, Just . ExprNode . target . unfocusCaseBind $ bz)

-- | Find a sub-type in a larger type by its ID.
findType :: ID -> Type -> Maybe Type
findType id ty = target <$> focusOnTy id ty

-- | An AST node tagged with its "sort" - i.e. if it's a type or expression or binding etc.
data SomeNode
  = ExprNode Expr
  | TypeNode Type
  | -- | If/when we model all bindings with 'Bind'', we will want to generalise this.
    CaseBindNode Bind
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON SomeNode
