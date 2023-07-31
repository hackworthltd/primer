module Tests.Eval.Utils (
  (~=),
  (~~=),
  genDirTm,
  testModules,
  hasTypeLets,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Generics.Uniplate.Data (universe, universeBi)
import Data.Map qualified as Map
import Hedgehog (PropertyT)
import Hedgehog.Gen qualified as Gen
import Primer.Core (
  Expr,
  Expr' (LetType),
  ID,
  Kind (KType),
  ModuleName (ModuleName),
  Type,
  Type' (TLet),
 )
import Primer.Core.DSL (create', lam, lvar, tcon, tfun)
import Primer.Core.Utils (forgetMetadata, forgetTypeMetadata, generateIDs)
import Primer.Def (ASTDef (ASTDef, astDefExpr, astDefType), Def (DefAST))
import Primer.Eval (Dir (Chk, Syn))
import Primer.Gen.Core.Typed (
  WT,
  forAllT,
  genChk,
  genSyn,
  genWTType,
 )
import Primer.Module (Module (Module, moduleDefs, moduleName, moduleTypes), builtinModule, primitiveModule)
import Primer.Primitives (tChar)
import Test.Tasty.HUnit (Assertion, (@?=))

-- | Generates
--
--  * a term (to be the subject of some evaluation steps)
--
-- Also returns
--
--  * whether the term is synthesisable or checkable
--
--  * the type of the term
genDirTm :: PropertyT WT (Dir, Expr, Type' ())
genDirTm = do
  dir <- forAllT $ Gen.element [Chk, Syn]
  (t', ty) <- case dir of
    Chk -> do
      ty' <- forAllT $ genWTType KType
      t' <- forAllT $ genChk ty'
      pure (t', ty')
    Syn -> forAllT genSyn
  t <- generateIDs t'
  pure (dir, t, ty)

-- | Some generally-useful globals to have around when testing.
-- Currently: an AST identity function on Char and all builtins and
-- primitives
testModules :: MonadFresh ID m => [m Module]
testModules = [builtinModule, pure primitiveModule, pure testModule]

testModule :: Module
testModule =
  let (ty, expr) = create' $ (,) <$> tcon tChar `tfun` tcon tChar <*> lam "x" (lvar "x")
   in Module
        { moduleName = ModuleName ["M"]
        , moduleTypes = mempty
        , moduleDefs =
            Map.singleton "idChar"
              $ DefAST
                ASTDef
                  { astDefType = ty
                  , astDefExpr = expr
                  }
        }

-- * Misc helpers

-- | Like '@?=' but specifically for expressions.
-- Ignores IDs and metadata.
(~=) :: HasCallStack => Expr -> Expr -> Assertion
x ~= y = forgetMetadata x @?= forgetMetadata y

-- | Like '~=' but for types.
(~~=) :: HasCallStack => Type -> Type -> Assertion
x ~~= y = forgetTypeMetadata x @?= forgetTypeMetadata y

-- | Does this expression have any unsupported-by-the-typechecker subterms?
-- These are @let@s binding type variables, either a 'LetType' in a term,
-- or a 'TLet' in an embedded type.
hasTypeLets :: Expr -> Bool
hasTypeLets e =
  not
    $ null [() | LetType{} <- universe e]
    && null [() | TLet{} <- universeBi @_ @Type e]
