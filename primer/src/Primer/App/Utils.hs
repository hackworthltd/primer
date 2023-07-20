{-# LANGUAGE OverloadedLabels #-}

module Primer.App.Utils (forgetProgTypecache)
where

import Foreword

import Optics (mapped, (%), (%~), (.~))
import Primer.App (Prog (..))
import Primer.Core (KindMeta, TypeMeta, _exprMeta, _exprTypeMeta, _type, _typeMeta)
import Primer.Def (ASTDef (..))
import Primer.Module (Module (..))
import Primer.TypeDef (ASTTypeDef (..), ValCon (..))

forgetProgTypecache :: Prog -> Prog
forgetProgTypecache =
  (#progImports % mapped %~ forgetMod)
    . (#progModules % mapped %~ forgetMod)
  where
    forgetMod :: Module -> Module
    forgetMod =
      (#moduleDefs % mapped % #_DefAST %~ forgetASTDef)
        . (#moduleTypes % mapped % #_TypeDefAST %~ forgetASTTypeDef)
    forgetASTDef :: ASTDef -> ASTDef
    forgetASTDef =
      (#astDefExpr % _exprMeta % _type .~ Nothing)
        . (#astDefExpr % _exprTypeMeta % _type .~ Nothing)
        . (#astDefType % _typeMeta % _type .~ Nothing)
    forgetASTTypeDef :: ASTTypeDef TypeMeta KindMeta -> ASTTypeDef TypeMeta KindMeta
    forgetASTTypeDef = #astTypeDefConstructors % mapped % #valConArgs % mapped % _typeMeta % _type .~ Nothing
