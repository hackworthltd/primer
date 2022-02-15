{-# LANGUAGE OverloadedLabels #-}

module Tests.Import where

import Control.Monad.Fresh
import qualified Data.Map.Strict as Map
import Foreword
import Optics
import Primer.App
import Primer.Core
import Primer.Name
import Primer.Primitives (allPrimTypeDefs)
import Test.Tasty.HUnit

unitDef :: ASTTypeDef
unitDef =
  ASTTypeDef
    { astTypeDefName = "Unit"
    , astTypeDefParameters = []
    , astTypeDefConstructors = [ValCon "MkUnit" []]
    , astTypeDefNameHints = []
    }

srcProg :: (Applicative m) => m Prog
srcProg =
  pure
    newEmptyProg
      { progTypes = [TypeDefAST natDef]
      , progDefs = mempty
      }

srcProg2 :: (MonadFresh ID m) => (Name, Name, Name) -> (Name, Name) -> m Prog
srcProg2 (listName, nilName, consName) (roseTyName, roseConName) = do
  let listTyDef =
        ASTTypeDef
          { astTypeDefName = listName
          , astTypeDefParameters = [("a", KType)]
          , astTypeDefConstructors =
              [ ValCon nilName []
              , ValCon consName [TVar () "a", TApp () (TCon () listName) (TVar () "a")]
              ]
          , astTypeDefNameHints = []
          }
  let roseTyDef =
        ASTTypeDef
          { astTypeDefName = roseTyName
          , astTypeDefParameters = [("a", KType)]
          , astTypeDefConstructors =
              [ ValCon
                  roseConName
                  [ TVar () "a"
                  , TApp () (TCon () listName) (TApp () (TCon () roseTyName) (TVar () "a"))
                  ]
              ]
          , astTypeDefNameHints = []
          }
  pure
    newEmptyProg
      { progTypes = [TypeDefAST listTyDef, TypeDefAST roseTyDef]
      , progDefs = mempty
      }

-- A program with all primitive types
-- and also String (which depends on "Char")
primSrcProg :: Applicative m => m Prog
primSrcProg =
  pure
    newEmptyProg
      { progTypes = map TypeDefAST [stringDef] <> map TypeDefPrim (Map.elems allPrimTypeDefs)
      }
  where
    stringDef :: ASTTypeDef
    stringDef =
      ASTTypeDef
        { astTypeDefName = "String"
        , astTypeDefParameters = []
        , astTypeDefConstructors = [ValCon "Empty" [], ValCon "HeadAnd" [TCon () "Char", TCon () "String"]]
        , astTypeDefNameHints = []
        }

-- Import without renaming works
unit_import_import_simple :: Assertion
unit_import_import_simple = runImportTest $ do
  p <- srcProg
  let iac =
        IAC
          { iacImportRenamingTypes = Map.singleton "Nat" ("Nat", Map.fromList [("Zero", "Zero"), ("Succ", "Succ")])
          , iacDepsTypes = mempty
          }
  importFromApp p iac
  result <- gets appProg
  return $ do
    progTypes result @?= progTypes p
    progDefs result @?= mempty

-- We can rename types and ctors when importing
unit_import_import_renaming :: Assertion
unit_import_import_renaming = runImportTest $ do
  p <- srcProg
  let iac =
        IAC
          { iacImportRenamingTypes = Map.singleton "Nat" ("N", Map.fromList [("Zero", "Z"), ("Succ", "S")])
          , iacDepsTypes = mempty
          }
  importFromApp p iac
  result <- gets appProg
  return $ do
    progTypes result
      @?= [ TypeDefAST $
              ASTTypeDef
                { astTypeDefName = "N"
                , astTypeDefParameters = []
                , astTypeDefConstructors = [ValCon "Z" [], ValCon "S" [TCon () "N"]]
                , astTypeDefNameHints = astTypeDefNameHints natDef
                }
          ]
    progDefs result @?= mempty

-- Importing and renaming primitives works
unit_import_primitive :: Assertion
unit_import_primitive = runImportTest $ do
  p <- primSrcProg
  let iac =
        IAC
          { iacImportRenamingTypes = Map.singleton "Char" ("Char", mempty)
          , iacDepsTypes = mempty
          }
  importFromApp p iac
  result <- gets appProg
  return $ do
    map typeDefName (progTypes result) @?= ["Char"]
    progDefs result @?= mempty

-- test deep subst in ctor types
unit_import_ctor_type :: Assertion
unit_import_ctor_type = runImportTest $ do
  p <- srcProg2 ("List", "Nil", "Cons") ("Rose", "MkRose")
  let iac =
        IAC
          { iacImportRenamingTypes =
              Map.fromList
                [ ("Rose", ("T", Map.fromList [("MkRose", "C")]))
                , ("List", ("List", Map.fromList [("Nil", "Nil"), ("Cons", "Cons")]))
                ]
          , iacDepsTypes = mempty
          }
  importFromApp p iac
  result <- gets appProg
  expect <- srcProg2 ("List", "Nil", "Cons") ("T", "C")
  return $ result @?= expect

-- test rewiring dependencies
-- Here we have mimiced importing a library 'A'
-- and then later a library 'B' which itself depends on 'A'
unit_import_rewire_deps :: Assertion
unit_import_rewire_deps = runImportTest $ do
  p <- srcProg2 ("List", "Nil", "Cons") ("Rose", "MkRose")
  let listRenaming = Map.singleton "List" ("L", Map.fromList [("Nil", "Nil"), ("Cons", "Cons")])
  let iac =
        IAC
          { iacImportRenamingTypes = listRenaming
          , iacDepsTypes = mempty
          }
  importFromApp p iac
  let iac' =
        IAC
          { iacImportRenamingTypes = Map.fromList [("Rose", ("T", Map.fromList [("MkRose", "C")]))]
          , iacDepsTypes = listRenaming
          }
  importFromApp p iac'
  result <- gets appProg
  expect <- srcProg2 ("L", "Nil", "Cons") ("T", "C")
  return $ result @?= expect

unit_import_rewire_primitive :: Assertion
unit_import_rewire_primitive = runImportTest $ do
  p <- primSrcProg
  let iac =
        IAC
          { iacImportRenamingTypes = Map.fromList [("Char", ("Char", mempty))]
          , iacDepsTypes = mempty
          }
  importFromApp p iac
  let iac' =
        IAC
          { iacImportRenamingTypes = Map.singleton "String" ("S", Map.fromList [("Empty", "Nil"), ("HeadAnd", "Cons")])
          , iacDepsTypes = Map.singleton "Char" ("Char", mempty)
          }
  importFromApp p iac'
  result <- gets appProg
  return $ do
    map typeDefName (progTypes result) @?= ["Char", "S"]
    progDefs result @?= mempty

-- cannot import without deps
unit_import_ref_not_handled :: Assertion
unit_import_ref_not_handled = runImportTestError (ReferencedTypeNotHandled "List") $ do
  p <- srcProg2 ("List", "Nil", "Cons") ("Rose", "MkRose")
  let iac =
        IAC
          { iacImportRenamingTypes = Map.fromList [("Rose", ("T", Map.fromList [("MkRose", "C")]))]
          , iacDepsTypes = mempty
          }
  importFromApp p iac

-- cannot claim to support deps with free name
unit_import_rewire_tgt_exist :: Assertion
unit_import_rewire_tgt_exist = runImportTestError (UnknownRewrittenTgtType "L") $ do
  p <- srcProg2 ("List", "Nil", "Cons") ("Rose", "MkRose")
  let iac =
        IAC
          { iacImportRenamingTypes = Map.fromList [("Rose", ("T", Map.fromList [("MkRose", "C")]))]
          , iacDepsTypes = Map.singleton "List" ("L", mempty)
          }
  importFromApp p iac

-- cannot claim to support deps with wrong kind
unit_import_rewire_kind_match :: Assertion
unit_import_rewire_kind_match = runImportTestError (RewriteTypeKindMismatch "List" "Unit") $ do
  _ <- handleMutationRequest $ Edit [AddTypeDef unitDef]
  p <- srcProg2 ("List", "Nil", "Cons") ("Rose", "MkRose")
  let iac =
        IAC
          { iacImportRenamingTypes = Map.fromList [("Rose", ("T", Map.fromList [("MkRose", "C")]))]
          , iacDepsTypes = Map.singleton "List" ("Unit", mempty)
          }
  importFromApp p iac

-- cannot import two types/ctors with the same name, or one the same as an existing one
unit_import_name_clash :: Assertion
unit_import_name_clash = do
  runImportTestError (DuplicateTypes ["T"]) $ do
    p <- srcProg2 ("List", "Nil", "Cons") ("Rose", "MkRose")
    let iac =
          IAC
            { iacImportRenamingTypes =
                Map.fromList
                  [ ("List", ("T", Map.fromList [("Nil", "Nil"), ("Cons", "Cons")]))
                  , ("Rose", ("T", Map.fromList [("MkRose", "MkRose")]))
                  ]
            , iacDepsTypes = mempty
            }
    importFromApp p iac
  runImportTestError (DuplicateTypes ["Unit"]) $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef unitDef]
    p <- p'
    let iac =
          IAC
            { iacImportRenamingTypes = Map.fromList [("T", ("Unit", Map.fromList [("A", "A"), ("B", "B")]))]
            , iacDepsTypes = mempty
            }
    importFromApp p iac
  runImportTestError (DuplicateCtors ["C"]) $ do
    p <- p'
    let iac =
          IAC
            { iacImportRenamingTypes = Map.fromList [("T", ("S", Map.fromList [("A", "C"), ("B", "C")]))]
            , iacDepsTypes = mempty
            }
    importFromApp p iac
  runImportTestError (DuplicateCtors ["MkUnit"]) $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef unitDef]
    p <- p'
    let iac =
          IAC
            { iacImportRenamingTypes = Map.fromList [("T", ("S", Map.fromList [("A", "A"), ("B", "MkUnit")]))]
            , iacDepsTypes = mempty
            }
    importFromApp p iac
  where
    p' :: (MonadFresh ID m) => m Prog
    p' = do
      let tyDep =
            ASTTypeDef
              { astTypeDefName = "T"
              , astTypeDefParameters = []
              , astTypeDefConstructors =
                  [ ValCon "A" []
                  , ValCon "B" []
                  ]
              , astTypeDefNameHints = []
              }
      pure
        newEmptyProg
          { progTypes = [TypeDefAST tyDep]
          , progDefs = mempty
          }

-- Test that rewiring dependencies only maps types to isomorphic types
unit_import_rewire_iso :: Assertion
unit_import_rewire_iso = do
  runImportTestError (RewriteCtorsNotSurjective "Unit" "Nat") $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef natDef]
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Unit" ("Nat", Map.singleton "MkUnit" "Zero")
            }
    importFromApp
      newEmptyProg
        { progTypes = [TypeDefAST unitDef]
        , progDefs = mempty
        }
      iac
  runImportTestError (RewriteCtorSrcNotExist "Nat" "Nonexistent") $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef natDef]
    p <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Nat" ("Nat", Map.fromList [("Zero", "Zero"), ("Succ", "Succ"), ("Nonexistent", "Nonexistent")])
            }
    importFromApp p iac
  runImportTestError (RewriteCtorTgtNotExist "Unit" "Succ") $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef unitDef]
    p <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Nat" ("Unit", Map.fromList [("Zero", "MkUnit"), ("Succ", "Succ")])
            }
    importFromApp p iac
  runImportTestError (DuplicateRewriteCtor "Nat" ["MkUnit"]) $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef unitDef]
    p <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Nat" ("Unit", Map.fromList [("Zero", "MkUnit"), ("Succ", "MkUnit")])
            }
    importFromApp p iac
  runImportTestError (RewrittenCtorTypeDiffers "Nat" "Succ" "Nat" "Zero") $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef natDef]
    p <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Nat" ("Nat", Map.fromList [("Zero", "Succ"), ("Succ", "Zero")])
            }
    importFromApp p iac

-- cannot both import and rename the same type
unit_import_import_rename_clash :: Assertion
unit_import_import_rename_clash = runImportTestError (ImportRenameType "Nat") $ do
  p <- srcProg
  let iac =
        IAC
          { iacImportRenamingTypes = Map.singleton "Nat" ("Nat", Map.fromList [("Zero", "Zero"), ("Succ", "Succ")])
          , iacDepsTypes = Map.singleton "Nat" ("Nat", Map.fromList [("Zero", "Zero"), ("Succ", "Succ")])
          }
  importFromApp p iac

-- cannot import/rewrite nonexistent type/ctor, and have to import/rewrite all ctors of a type
unit_import_nonexist :: Assertion
unit_import_nonexist = do
  runImportTestError (UnknownImportedType "Nonexistent") $ do
    p <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Nonexistent" ("T", mempty)
            , iacDepsTypes = mempty
            }
    importFromApp p iac
  runImportTestError (UnknownImportedCtor "Nat" "Nonexistent") $ do
    p <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Nat" ("Nat", Map.fromList [("Zero", "Zero"), ("Succ", "Succ"), ("Nonexistent", "C")])
            , iacDepsTypes = mempty
            }
    importFromApp p iac
  runImportTestError (MissedImportCtor "Nat" "Succ") $ do
    p <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Nat" ("Nat", Map.fromList [("Zero", "Zero")])
            , iacDepsTypes = mempty
            }
    importFromApp p iac
  runImportTestError (UnknownRewrittenSrcType "Nonexistent") $ do
    p <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Nonexistent" ("T", mempty)
            }
    importFromApp p iac
  runImportTestError (RewriteCtorSrcNotExist "Nat" "Nonexistent") $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef natDef]
    p <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Nat" ("Nat", Map.fromList [("Zero", "Zero"), ("Succ", "Succ"), ("Nonexistent", "C")])
            }
    importFromApp p iac
  runImportTestError (RewriteCtorTgtNotExist "Unit" "Nonexistent") $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef unitDef]
    p <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Nat" ("Unit", Map.fromList [("Zero", "MkUnit"), ("Succ", "Nonexistent")])
            }
    importFromApp p iac
  runImportTestError (MissedRewriteCtor "Nat" "Succ") $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef unitDef]
    p <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Nat" ("Unit", Map.fromList [("Zero", "MkUnit")])
            }
    importFromApp p iac

-- Cannot rewrite a ctor to ctor of different type
-- this will hit "unknown ctor" error
unit_import_rewire_cross_type :: Assertion
unit_import_rewire_cross_type = do
  runImportTestError (RewriteCtorTgtNotExist "Unit" "Succ") $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef unitDef]
    p <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Nat" ("Unit", Map.fromList [("Zero", "MkUnit"), ("Succ", "Succ")])
            }
    importFromApp p iac

-- Cannot rename primitive types (either in import or rewire)
unit_import_rename_prim_type :: Assertion
unit_import_rename_prim_type = do
  runImportTestError (CannotRenamePrimitiveType "Char") $ do
    p <- primSrcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Char" ("C", mempty)
            , iacDepsTypes = mempty
            }
    importFromApp p iac
  runImportTestError (CannotRenamePrimitiveType "Char") $ do
    p <- primSrcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Int" ("Int", mempty)
            , iacDepsTypes = mempty
            }
    importFromApp p iac
    let iac' =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Char" ("Int", mempty)
            }
    importFromApp p iac'

unit_import_prim_type_ctor :: Assertion
unit_import_prim_type_ctor = do
  runImportTestError (PrimitiveTypeHasNoCtor "Char") $ do
    p <- primSrcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Char" ("Char", Map.singleton "a" "a")
            , iacDepsTypes = mempty
            }
    importFromApp p iac
  runImportTestError (PrimitiveTypeHasNoCtor "Char") $ do
    p <- primSrcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Char" ("Char", Map.singleton "a" "a")
            , iacDepsTypes = mempty
            }
    importFromApp p iac
    let iac' =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Char" ("Char", Map.singleton "a" "a")
            }
    importFromApp p iac'

-- Cannot rewire a primitive type to a user-defined type of the same name (or vv)
unit_import_prim_user_type :: Assertion
unit_import_prim_user_type = do
  runImportTestError (RewriteTypePrimitiveMismatch "Char" "Char") $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef unitDef{astTypeDefName = "Char"}]
    p <- primSrcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Char" ("Char", mempty)
            }
    importFromApp p iac
  runImportTestError (RewriteTypePrimitiveMismatch "Char" "Char") $ do
    p <- primSrcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Char" ("Char", mempty)
            , iacDepsTypes = mempty
            }
    importFromApp p iac
    let iac' =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Char" ("Char", mempty)
            }
    importFromApp p' iac'
  where
    p' =
      newEmptyProg
        { progTypes = [TypeDefAST unitDef{astTypeDefName = "Char"}]
        }

-- runs in an initially empty prog. you can add stuff monadically, I hope... not yet tried to use that feature
runImportTest :: EditAppM Assertion -> Assertion
runImportTest ma = case runEditAppM ma reallyEmptyApp of
  (Left err, _) -> assertFailure $ "running actions failed! " <> show err
  (Right a, _) -> a

runImportTestError :: ImportError -> EditAppM () -> Assertion
runImportTestError expect ma = case runEditAppM ma reallyEmptyApp of
  (Left err, _) -> err @?= ImportError expect
  (Right _, _) -> assertFailure $ "running actions unexpectedly succeeded, expected " <> show expect

reallyEmptyApp :: App
reallyEmptyApp = newEmptyApp & #appProg % #progDefs .~ mempty
