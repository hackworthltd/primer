{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Let (
  LetRemovalDetail (..),
  tryLetRemoval,
  findFreeOccurrencesExpr,
  findFreeOccurrencesType,
) where

import Foreword

import Control.Arrow ((***))
import Control.Monad.Fresh (MonadFresh)
import Data.Set qualified as Set
import Optics (filtered, getting, notElemOf, to, (%), (^.), (^..), _1, _2)
import Primer.Core (
  Expr,
  Expr' (Let, LetType, Letrec),
  ID,
  LocalName (unLocalName),
  TyVarName,
  Type,
  getID,
  _id,
 )
import Primer.Core.DSL (letType, let_)
import Primer.Core.Utils (freeVars, freeVarsTy, _freeTmVars, _freeTyVars, _freeVars, _freeVarsTy)
import Primer.Eval.Bind (BindRenameDetail (..))
import Primer.Eval.Utils (makeSafeLetBinding, makeSafeLetTypeBinding)
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (Name)

-- | Detailed information about a removal of a let binding.
-- This can be any of: a term-level non-recursive let, a
-- term-level recursive let, a term-level let binding a type
-- or a type-level let.
-- If term-level: t ~ Expr; if type-level: t ~ Type
data LetRemovalDetail t = LetRemovalDetail
  { before :: t
  -- ^ the let expression before reduction
  , after :: t
  -- ^ the resulting expression after reduction
  , bindingName :: Name
  -- ^ the name of the unused bound variable (either term or type variable)
  , letID :: ID
  -- ^ the full let expression
  , bodyID :: ID
  -- ^ the right hand side of the let
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (LetRemovalDetail t)

tryLetRemoval ::
  MonadFresh ID m =>
  Expr ->
  Maybe (m (Expr, Either (BindRenameDetail Expr) (LetRemovalDetail Expr)))
tryLetRemoval = \case
  expr@(Let meta x e body)
    -- Redundant let removal
    -- let x = e1 in e2 ==> e2    if x not free in e2
    | notElemOf (getting _freeTmVars % _2) x body -> mkLetRemovalDetail expr body x meta
    -- Renaming a potentially self-capturing let
    -- let x = f[x] in g[x] ==> let y = f[x] in g[y]
    | otherwise -> mkLetRenameDetail expr body (Left (x, e)) meta
  expr@(Letrec meta x _ _ body)
    -- Redundant letrec removal
    -- letrec x = e in e2 ==> e2  if x not free in e2
    | notElemOf (getting _freeTmVars % _2) x body -> mkLetRemovalDetail expr body x meta
  expr@(LetType meta x t body)
    -- Redundant letType removal
    -- let type x = t in e ==> e  if x not free in e
    | notElemOf (getting _freeTyVars % _2) x body -> mkLetRemovalDetail expr body x meta
    -- Renaming a potentially self-capturing letType
    -- let type x = f[x] in g[x] ==> let type y = f[x] in g[y]
    | otherwise -> mkLetRenameDetail expr body (Right (x, t)) meta
  _ -> Nothing
  where
    mkLetRemovalDetail expr body x meta =
      Just $
        pure
          ( body
          , Right $
              LetRemovalDetail
                { before = expr
                , after = body
                , bindingName = unLocalName x
                , letID = meta ^. _id
                , bodyID = body ^. _id
                }
          )
    mkLetRenameDetail expr body binding meta = Just $ do
      (x, y, occ, expr') <- case binding of
        Left (x, e) -> do
          let (y, body') = makeSafeLetBinding x (freeVars e) body
          let occ = findFreeOccurrencesExpr x e
          (unLocalName x,unLocalName y,occ,) <$> let_ y (pure e) (pure body')
        Right (x, ty) -> do
          let (y, body') = makeSafeLetTypeBinding x (Set.map unLocalName $ freeVarsTy ty) body
          let occ = findFreeOccurrencesType x ty
          (unLocalName x,unLocalName y,occ,) <$> letType y (pure ty) (pure body')
      pure
        ( expr'
        , Left $
            BindRenameDetail
              { before = expr
              , after = expr'
              , bindingNamesOld = [x]
              , bindingNamesNew = [y]
              , bindersOld = [meta ^. _id]
              , bindersNew = [getID expr']
              , bindingOccurrences = occ
              , renamingLets = Nothing
              , bodyID = body ^. _id
              }
        )

findFreeOccurrencesExpr :: LocalName k -> Expr -> [ID]
findFreeOccurrencesExpr x e = e ^.. _freeVars % to idName % filtered ((== unLocalName x) . snd) % _1
  where
    idName = either (getID *** unLocalName) (getID *** unLocalName)

findFreeOccurrencesType :: TyVarName -> Type -> [ID]
findFreeOccurrencesType x ty = ty ^.. getting _freeVarsTy % to (first getID) % filtered ((== x) . snd) % _1
