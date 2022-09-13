module Primer.Eval.Utils (
  annOf,
  annotate,
  makeSafeLetBinding,
  makeSafeLetTypeBinding,
  makeSafeTLetBinding,
) where

import Foreword

import Data.Set qualified as Set
import Optics (set, view, (%))
import Primer.Core (
  Expr,
  LVarName,
  LocalName (LocalName, unLocalName),
  Meta,
  TyVarName,
  Type,
  TypeCache,
  _exprMetaLens,
  _type,
 )
import Primer.Core.Transform (renameLocalVar, renameTyVar, renameTyVarExpr)
import Primer.Name (Name (unName), unsafeMkName)

-- | This function helps us create let bindings which are easy to substitute
-- without causing variable capture.
-- It takes as arguments:
-- - a variable name, @x@ (the variable we would like to bind)
-- - some names to avoid @vs@ (normally the free (type and term) variables in
--   the term we would like to bind)
-- - the term @t[x]@ we would like the binding to scope over (in which which
--   @x@ presumably appears free)
-- It will then modify the original name until it finds one that:
-- - doesn't clash with any of the @vs@
-- - can be safely used instead of the original name in the lambda body
-- Thus, it will return @y@ and @t[y]@ for some @y@ distinct from each
-- of the @vs@, and such that the renaming is trivial (i.e. we do not need to
-- alpha convert any binders in @t@ to avoid capture).
--
-- We assume that the original name is safe to use, so we return it
-- unchanged if it doesn't clash with a free variable in the argument.
--
-- The reason this eases future substitution is that we avoid making terms such
-- as @let x = C x in D x x@ where you cannot inline just one occurrence of the
-- @x@ without causing capture.
--
-- See 'Tests.Eval.unit_tryReduce_beta_name_clash' for an example of where this is useful.
makeSafeLetBinding :: LVarName -> Set Name -> Expr -> (LVarName, Expr)
makeSafeLetBinding = makeSafeLetBinding' renameLocalVar

-- | As 'makeSafeLetBinding', but for Λ applications
makeSafeLetTypeBinding :: TyVarName -> Set Name -> Expr -> (TyVarName, Expr)
makeSafeLetTypeBinding = makeSafeLetBinding' renameTyVarExpr

-- Helper for makeSafeLet{,Type}Binding
makeSafeLetBinding' ::
  (LocalName k -> LocalName k -> Expr -> Maybe Expr) ->
  LocalName k ->
  Set Name ->
  Expr ->
  (LocalName k, Expr)
makeSafeLetBinding' _ name others body | Set.notMember (unLocalName name) others = (name, body)
makeSafeLetBinding' rename name others body = go 0
  where
    go :: Int -> (LocalName k, Expr)
    go n =
      let newName' = unsafeMkName $ unName (unLocalName name) <> show n
          newName = LocalName newName'
       in if Set.member newName' others
            then go (n + 1)
            else case rename name newName body of
              Just body' -> (newName, body')
              Nothing -> go (n + 1)

makeSafeTLetBinding ::
  TyVarName ->
  Set TyVarName ->
  Type ->
  (TyVarName, Type)
makeSafeTLetBinding name others body | Set.notMember name others = (name, body)
makeSafeTLetBinding name others body = go 0
  where
    go :: Int -> (TyVarName, Type)
    go n =
      let newName = LocalName . unsafeMkName $ unName (unLocalName name) <> show n
       in if Set.member newName others
            then go (n + 1)
            else case renameTyVar name newName body of
              Just body' -> (newName, body')
              Nothing -> go (n + 1)

-- | Extract the cached type information from the metadata of an AST node.
annOf :: Meta a -> a
annOf = view _type

-- | Set the cached type information of the root node of the given expression to the given value.
annotate :: Maybe TypeCache -> Expr -> Expr
annotate = set (_exprMetaLens % _type)
