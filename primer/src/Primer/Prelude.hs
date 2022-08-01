import Control.Monad.Fresh
import Foreword hiding (not)
import Primer.Builtins qualified as B
import Primer.Core
import Primer.Core.DSL

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
