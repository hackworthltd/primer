{-# LANGUAGE OverloadedLabels #-}

-- | This module contains the zipper types @ExprZ@ and @TypeZ@, and functions for
--  operating on them.
module Primer.Zipper (
  ExprZ,
  TypeZip,
  TypeZ,
  KindZ,
  KindTZ,
  CaseBindZ,
  updateCaseBind,
  unfocusCaseBind,
  caseBindZFocus,
  caseBindZMeta,
  IsZipper (asZipper),
  Loc,
  Loc' (..),
  BindLoc,
  BindLoc' (..),
  focusType,
  focusLoc,
  unfocusType,
  unfocusKind,
  unfocusKindT,
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
  foldBelow,
  unfocusExpr,
  unfocusLoc,
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
  singular,
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
  Expr' (Case, Con, LAM, Lam, Let, LetType, Letrec),
  ExprMeta,
  HasID (..),
  ID,
  Kind',
  KindMeta,
  LVarName,
  LocalName (unLocalName),
  Type,
  Type' (),
  TypeMeta,
  bindName,
  getID,
  typesInExpr,
  _bindMeta,
 )
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (Name)
import Primer.Zipper.Nested (
  IsZipper (..),
  ZipNest (ZipNest),
  down,
  focus,
  innerZipNest,
  left,
  mergeNest,
  replace,
  right,
  target,
  top,
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
  TypeZip,
  TypeZip',
  bindersAboveTy,
  bindersBelowTy,
  farthest,
  focusOnKind,
  focusOnTy,
  focusOnTy',
  foldAbove,
  foldBelow,
  getBoundHereDnTy,
  getBoundHereTy,
  getBoundHereUpTy,
  search,
  unfocusKindT,
 )

type ExprZ' a b c = Zipper (Expr' a b c) (Expr' a b c)

-- | An ordinary zipper for 'Expr's
type ExprZ = ExprZ' ExprMeta TypeMeta ()

-- | A zipper for 'Type's embedded in expressions.
-- For such types, we need a way
-- to navigate around them without losing our place in the wider expression.
-- This type contains a Zipper for a 'Type' and a function that will place the
-- unzippered type back into the wider expression zipper, keeping its place.
type TypeZ' a b c = ZipNest (ExprZ' a b c) (TypeZip' b c) (Type' b c)

type TypeZ = TypeZ' ExprMeta TypeMeta ()

-- | A zipper for 'Kind's embedded in expressions (which will always be inside a 'Type').
type KindZ' a b c = ZipNest (ExprZ' a b c) (KindTZ' b c) (Type' b c)

type KindZ = KindZ' ExprMeta TypeMeta KindMeta

-- | A zipper for variable bindings in case branches.
-- This type focuses on a particular binding in a particular branch.
-- It contains the focused binding, along with the the parent expression (as a zipper) and some
-- parts of the surrounding branch which are useful when renaming.
-- It also contains a function which can update the binding and the RHS of the case branch
-- simultaneously, yielding a new expression.
-- These fields are chosen to be convenient for renaming, and they may not be that useful for future
-- actions we want to perform.
data CaseBindZ' a b c = CaseBindZ
  { caseBindZExpr :: ExprZ' a b c
  -- ^ a zipper focused on the case expression
  , caseBindZFocus :: Bind' a
  -- ^ the focused binding
  , caseBindZRhs :: Expr' a b c
  -- ^ the rhs of the branch
  , caseBindAllBindings :: [Bind' a]
  -- ^ all other bindings in the case branch, i.e. all except the focused one
  , caseBindZUpdate :: Bind' a -> Expr' a b c -> ExprZ' a b c -> ExprZ' a b c
  -- ^ a function to update the focused binding and rhs simultaneously
  }
  deriving stock (Generic)

type CaseBindZ = CaseBindZ' ExprMeta TypeMeta ()

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

instance HasID a => HasID (CaseBindZ' a b c) where
  _id = #caseBindZFocus % _id

caseBindZMeta :: Lens' (CaseBindZ' a b c) a
caseBindZMeta = #caseBindZFocus % _bindMeta

-- | A specific location in our AST.
-- This can either be in an expression, type, or binding.
data Loc' a b c
  = -- | An expression
    InExpr (ExprZ' a b c)
  | -- | A type
    InType (TypeZ' a b c)
  | -- | A kind
    -- (This temporarily has an extra 'Void' field, as we cannot yet construct them.
    -- This acts to stub out some definitions that do not yet make sense as we currently
    -- set @c~()@ in 'Loc'; in particular, we want @HasID Loc@.)
    InKind (KindZ' a b c) Void
  | -- | A binding (currently just case bindings)
    InBind (BindLoc' a b c)
  deriving stock (Generic)

type Loc = Loc' ExprMeta TypeMeta ()

instance (HasID a, HasID b) => HasID (Loc' a b c) where
  _id = lens getter setter
    where
      getter = \case
        InExpr e -> view _id e
        InType l -> view _id l
        InKind _ v -> absurd v
        InBind l -> view _id l
      setter l i = case l of
        InExpr e -> InExpr $ set _id i e
        InType t -> InType $ set _id i t
        InKind _ v -> absurd v
        InBind t -> InBind $ set _id i t

-- | A location of a binding.
-- This only covers bindings in case branches for now.

{- HLINT ignore BindLoc' "Use newtype instead of data" -}
data BindLoc' a b c
  = BindCase (CaseBindZ' a b c)
  deriving stock (Generic)

type BindLoc = BindLoc' ExprMeta TypeMeta

instance HasID a => HasID (BindLoc' a b c) where
  _id = position @1 % _id

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

-- | If the currently focused expression is a case expression, search the bindings of its branches
-- to find one matching the given ID, and return the 'Loc' for that binding.
-- If no match is found, return @Nothing@.
findInCaseBinds :: forall a b c. (Data a, Data b, Data c, Eq a, HasID a) => ID -> ExprZ' a b c -> Maybe (Loc' a b c)
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
    branchesLens :: AffineTraversal' (ExprZ' a b c) [CaseBranch' a b c]
    branchesLens = _target % #_Case % _3
    binds :: Lens' (CaseBranch' a b c) [Bind' a]
    binds = position @2
    branchRHS :: Lens' (CaseBranch' a b c) (Expr' a b c)
    branchRHS = position @3

-- | Switch from a 'Type' zipper back to an 'Expr' zipper.
unfocusType :: (Data b, Data c) => TypeZ' a b c -> ExprZ' a b c
unfocusType = unfocusNest

-- | Switch from a 'Kind'-in-'Type'-in-'Expr' zipper back to an 'Type'-in-'Expr' zipper.
unfocusKind :: Data c => KindZ' a b c -> TypeZ' a b c
unfocusKind = mergeNest

-- | Forget the surrounding expression context
focusOnlyType :: TypeZ' a b c -> TypeZip' b c
focusOnlyType = innerZipNest

-- 'CaseBindZ' is sort of a fake zipper which can only focus on one thing: the case binding.
-- It's a bit fiddly to make it appear as a zipper like this, but it's convenient to have a
-- consistent interface for 'ExprZ', 'TypeZ' and 'CaseBindZ'.
instance IsZipper CaseBindZ (Bind' ExprMeta) where
  asZipper = #caseBindZFocus % iso zipper fromZipper

-- | Convert an 'Expr' to a 'Loc' which focuses on the top of the expression.
focusLoc :: Expr -> Loc
focusLoc = InExpr . focus

-- Convert a 'CaseBindZ' to an 'ExprZ' by shifting focus to the parent case expression.
unfocusCaseBind :: CaseBindZ' a b c -> ExprZ' a b c
unfocusCaseBind = caseBindZExpr

-- | Convert an 'Expr' zipper to an 'Expr'
unfocusExpr :: ExprZ' a b c -> Expr' a b c
unfocusExpr = fromZipper

-- | Convert a 'Loc' to an 'ExprZ'.
-- If we're in a type or kind or case binding, we'll shift focus up to the nearest enclosing expression.
unfocusLoc :: Loc -> ExprZ
unfocusLoc (InExpr z) = z
unfocusLoc (InType z) = unfocusType z
unfocusLoc (InBind (BindCase z)) = unfocusCaseBind z
unfocusLoc (InKind k _) = unfocusType $ unfocusKind k

-- | Convert a 'Loc' to an 'Expr'.
-- This shifts focus right up to the top, so the result is the whole expression.
unfocus :: Loc -> Expr
unfocus = unfocusExpr . unfocusLoc

-- | Focus on the node with the given 'ID', if it exists in the expression
focusOn :: (Data a, Data b, Eq a, HasID a, HasID b, c ~ ()) => ID -> Expr' a b c -> Maybe (Loc' a b c)
focusOn i = focusOn' i . focus

-- | Focus on the node with the given 'ID', if it exists in the focussed expression
focusOn' :: (Data a, Data b, Eq a, HasID a, HasID b, c ~ ()) => ID -> ExprZ' a b c -> Maybe (Loc' a b c)
focusOn' i = fmap snd . search matchesID
  where
    matchesID z
      -- If the current target has the correct ID, return that
      | getID (target z) == i = Just $ InExpr z
      -- If the target has an embedded type, search the type for a match.
      -- If the target is a case expression with bindings, search each binding for a match.
      | otherwise =
          let inType = do
                ZipNest tz f <- focusType z
                focusOnTy' i tz <&> \case
                  Left tz' -> InType $ ZipNest tz' f
                  Right (kz, v) -> InKind (ZipNest kz f) v
              inCaseBinds = findInCaseBinds i z
           in inType <|> inCaseBinds

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

-- Get the names bound within the focussed subtree
bindersBelow :: ExprZ -> S.Set Name
bindersBelow = foldBelow getBoundHereDn

-- Get all names bound by this layer of an expression, for any child.
-- E.g. for a "match" we get all vars bound by each branch.
getBoundHereDn :: (Eq a, Eq b, Eq c) => Expr' a b c -> S.Set Name
getBoundHereDn e = getBoundHere e Nothing

-- Get the names bound by this layer of an expression (both term and type names)
-- The second arg is the child we just came out of, if traversing up (and thus
-- need to extract binders based on which case branch etc), and Nothing if
-- traversing down (and want to get all binders regardless of branch).
getBoundHere :: (Eq a, Eq b, Eq c) => Expr' a b c -> Maybe (Expr' a b c) -> S.Set Name
getBoundHere e prev = S.fromList $ either identity letBindingName <$> getBoundHere' e prev

data LetBinding' a b c
  = LetBind LVarName (Expr' a b c)
  | LetrecBind LVarName (Expr' a b c) (Type' b c)
  | LetTyBind (LetTypeBinding' b c)
  deriving stock (Eq, Show)
type LetBinding = LetBinding' ExprMeta TypeMeta ()

letBindingName :: LetBinding' a b c -> Name
letBindingName = \case
  LetBind n _ -> unLocalName n
  LetrecBind n _ _ -> unLocalName n
  LetTyBind (LetTypeBind n _) -> unLocalName n

getBoundHere' :: (Eq a, Eq b, Eq c) => Expr' a b c -> Maybe (Expr' a b c) -> [Either Name (LetBinding' a b c)]
getBoundHere' e prev = case e of
  Lam _ v _ -> anon v
  LAM _ tv _ -> anon tv
  Let _ v rhs b ->
    if maybe True (== b) prev
      then letBind $ LetBind v rhs
      else mempty
  Letrec _ v rhs t _ -> letBind $ LetrecBind v rhs t
  LetType _ v t _ -> letBind $ LetTyBind $ LetTypeBind v t
  Case _ _ bs _ ->
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
    InKind kz v ->
      ( KindNode (target kz) v
      , Just $ maybe (TypeNode $ target $ unfocusKind kz) (flip KindNode v . target) $ up kz
      )
    InBind (BindCase bz) -> (CaseBindNode $ caseBindZFocus bz, Just . ExprNode . target . unfocusCaseBind $ bz)

-- | Find a sub-type or kind in a larger type by its ID.
findTypeOrKind :: (Data a, HasID a, b ~ ()) => ID -> Type' a b -> Maybe (Either (Type' a b) (Kind' b))
findTypeOrKind id ty = bimap target (target . fst) <$> focusOnTy id ty

-- | Find a sub-type in a larger type by its ID.
findType :: (Data a, HasID a, b ~ ()) => ID -> Type' a b -> Maybe (Type' a b)
findType id ty = findTypeOrKind id ty >>= leftToMaybe

-- | An AST node tagged with its "sort" - i.e. if it's a type or expression or binding etc.
data SomeNode
  = ExprNode Expr
  | TypeNode Type
  | KindNode (Kind' ()) Void -- Void here for similar reasons as in Loc
  | -- | If/when we model all bindings with 'Bind'', we will want to generalise this.
    CaseBindNode Bind
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON SomeNode
