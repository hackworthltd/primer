{-# LANGUAGE OverloadedLabels #-}

module Primer.App.Utils (forgetProgTypecache)
where

import Foreword

import Optics (mapped, (%), (%~), (.~))
import Primer.App (Prog (..))
import Primer.Core (_exprMeta, _exprTypeMeta, _type, _typeMeta)
import Primer.Def (ASTDef (..))
import Primer.Module (Module (..))

forgetProgTypecache :: Prog -> Prog
forgetProgTypecache =
  (#progImports % mapped %~ forgetMod)
    . (#progModules % mapped %~ forgetMod)
  where
    forgetMod :: Module -> Module
    forgetMod = #moduleDefs % mapped % #_DefAST %~ forgetASTDef
    forgetASTDef :: ASTDef -> ASTDef
    forgetASTDef =
      (#astDefExpr % _exprMeta % _type .~ Nothing)
        . (#astDefExpr % _exprTypeMeta % _type .~ Nothing)
        . (#astDefType % _typeMeta % _type .~ Nothing)
