import Control.Monad.Fresh
import Data.Foldable (foldl1)
import Primer.Builtins qualified as B
import Primer.Core
import Primer.Core.DSL
import Data.Foldable (foldl1)

modName :: ModuleName
modName = mkSimpleModuleName "Prelude"

not :: GVarName
not = qualifyName modName "not"

-- | The function `not :: Bool -> Bool`.
notDef :: MonadFresh ID m => m Def
notDef = do
  type_ <- tcon B.tBool `tfun` tcon B.tBool
  term <-
    lam
      "x"
      ( case_
          (lvar "x")
          [ branch B.cTrue [] (con B.cFalse)
          , branch B.cFalse [] (con B.cTrue)
          ]
      )
  pure $ DefAST $ ASTDef term type_

and :: GVarName
and = qualifyName modName "and"

andDef :: MonadFresh ID m => m Def
andDef = do
  type_ <- tcon B.tBool `tfun` (tcon B.tBool `tfun` tcon B.tBool)
  term <-
    lam
      "x"
      ( case_
          (lvar "x")
          [ branch
              B.cTrue
              []
              ( lam
                  "y"
                  ( case_
                      (lvar "y")
                      [ branch B.cTrue [] (con B.cTrue)
                      , branch B.cFalse [] (con B.cFalse)
                      ]
                  )
              )
          , branch B.cFalse [] (lam "y" $ con B.cFalse)
          ]
      )
  pure $ DefAST $ ASTDef term type_

or :: GVarName
or = qualifyName modName "or"

orDef :: MonadFresh ID m => m Def
orDef = do
  type_ <- tcon B.tBool `tfun` (tcon B.tBool `tfun` tcon B.tBool)
  term <-
    lam
      "x"
      ( case_
          (lvar "x")
          [ branch B.cTrue [] (lam "y" $ con B.cTrue)
          , branch
              B.cFalse
              []
              ( lam
                  "y"
                  ( case_
                      (lvar "y")
                      [ branch B.cTrue [] $ con B.cTrue
                      , branch B.cFalse [] $ con B.cFalse
                      ]
                  )
              )
          ]
      )
  pure $ DefAST $ ASTDef term type_

-- | Helper function for functions with multiple arguments
-- Unsafe - do not call with empty list
appFold :: (Foldable t, MonadFresh ID m) => t (m Expr) -> m Expr
appFold = foldl1 app

