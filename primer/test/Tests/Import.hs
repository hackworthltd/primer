{-# LANGUAGE OverloadedLabels #-}

module Tests.Import where

import Control.Monad.Fresh
import qualified Data.Map.Strict as Map
import Foreword
import Optics
import Primer.App
import Primer.Core
import Primer.Core.DSL
import Primer.Core.Utils (noHoles, noHolesTm)
import Primer.Name
import Primer.Primitives (allPrimDefs, allPrimTypeDefs)
import Primer.Typecheck (mkDefMap)
import Test.Tasty.HUnit

unitDef :: ASTTypeDef
unitDef =
  ASTTypeDef
    { astTypeDefName = "Unit"
    , astTypeDefParameters = []
    , astTypeDefConstructors = [ValCon "MkUnit" []]
    , astTypeDefNameHints = []
    }

srcProg :: (MonadFresh ID m) => m (Prog, ID, ID)
srcProg = do
  plusId <- fresh
  plusType <- tcon "Nat" `tfun` (tcon "Nat" `tfun` tcon "Nat")
  plusExpr <-
    lam "x" $
      lam "y" $
        case_
          (var "y")
          [ branch "Zero" [] $ var "x"
          , branch "Succ" [("z", Nothing)] $
              con "Succ" `app` (global plusId `app` var "x" `app` var "z")
          ]
  let plus =
        ASTDef
          { astDefID = plusId
          , astDefName = "plus"
          , astDefType = plusType
          , astDefExpr = plusExpr
          }
  multId <- fresh
  multType <- tcon "Nat" `tfun` (tcon "Nat" `tfun` tcon "Nat")
  multExpr <-
    lam "x" $
      lam "y" $
        case_
          (var "y")
          [ branch "Zero" [] $ con "Zero"
          , branch "Succ" [("z", Nothing)] $
              global plusId `app` var "x" `app` (global multId `app` var "x" `app` var "y")
          ]
  let mult =
        ASTDef
          { astDefID = multId
          , astDefName = "mult"
          , astDefType = multType
          , astDefExpr = multExpr
          }
  pure
    ( newEmptyProg
      { progTypes = [TypeDefAST natDef]
      , progDefs = mkDefMap $ DefAST <$> [plus, mult]
      }
    , plusId
    , multId
    )

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

-- A program with all primitive types and definitions,
-- along with a map of primitive-definition-name to primitive-definition-id
-- and the id of one non-primitive def, that depends on
-- primitive types "Char" and terms "eqChar", "toUpper"
-- and also Bool and String (which depends on "Char")
primSrcProg :: (MonadFresh ID m) => m (Prog, Map Name ID, ID)
primSrcProg = do
  primGlobals <- for (Map.toList allPrimDefs) $ \(n, f) -> do
    i <- fresh
    t <- primFunType f
    pure (t, n, i)
  let (primNames, primMap) = flip foldMap primGlobals $ \(t, n, i) ->
        (Map.singleton n i, Map.singleton i $ DefPrim $ PrimDef i n t)
  wrapperId <- fresh
  wrapperType <- tcon "Char" `tfun` tcon "Bool"
  wrapperExpr <-
    lam "x" $
      global (primNames Map.! "eqChar")
        `app` (global (primNames Map.! "toUpper") `app` var "x")
        `app` char 'a'
  let wrapper =
        ASTDef
          { astDefID = wrapperId
          , astDefName = "wrappedPrim"
          , astDefType = wrapperType
          , astDefExpr = wrapperExpr
          }
  pure
    ( newEmptyProg
      { progTypes = map TypeDefAST [boolDef, stringDef] <> map TypeDefPrim (Map.elems allPrimTypeDefs)
      , progDefs = primMap <> Map.singleton wrapperId (DefAST wrapper)
      }
    , primNames
    , wrapperId
    )
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
  (p, plusID, _) <- srcProg
  let iac =
        IAC
          { iacImportRenamingTypes = Map.singleton "Nat" ("Nat", Map.fromList [("Zero", "Zero"), ("Succ", "Succ")])
          , iacDepsTypes = mempty
          , iacImportRenamingTerms = Map.singleton plusID "plus"
          , iacDepsTerms = mempty
          }
  importFromApp p iac
  result <- gets appProg
  return $ do
    progTypes result @?= progTypes p
    -- It is difficult to say exactly what the imported terms are without
    -- basically doing the import-renaming again. We instead just check
    -- we have the expected definitions, and they do not contain any holes
    -- (as one may worry would happen if they were imported badly, and then
    -- smartholes kicked in)
    assertBool "There are holes in the resultant program" $
      all holeFree (Map.elems $ progDefs result)
    defName <$> Map.elems (progDefs result) @?= ["plus"]

holeFree :: Def -> Bool
holeFree def =
  noHoles (defType def)
    && maybe True noHolesTm (def ^? #_DefAST % #astDefExpr)

-- We can rename types and ctors when importing
unit_import_import_renaming :: Assertion
unit_import_import_renaming = runImportTest $ do
  (p, _, _) <- srcProg
  let iac =
        IAC
          { iacImportRenamingTypes = Map.singleton "Nat" ("N", Map.fromList [("Zero", "Z"), ("Succ", "S")])
          , iacDepsTypes = mempty
          , iacImportRenamingTerms = mempty
          , iacDepsTerms = mempty
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
  (p, prims, fooId) <- primSrcProg
  let iac =
        IAC
          { iacImportRenamingTypes =
              Map.fromList
                [ ("Char", ("Char", mempty))
                , ("Bool", ("B", Map.fromList [("True", "tt"), ("False", "ff")]))
                ]
          , iacDepsTypes = mempty
          , iacImportRenamingTerms = Map.fromList [(prims Map.! "toUpper", "UPPER"), (prims Map.! "eqChar", "=="), (fooId, "foo")]
          , iacDepsTerms = mempty
          }
  importFromApp p iac
  result <- gets appProg
  return $ do
    map typeDefName (progTypes result) @?= ["B", "Char"]
    assertBool "There are holes in the resultant program" $
      all holeFree (Map.elems $ progDefs result)
    defName <$> Map.elems (progDefs result) @?= ["==", "UPPER", "foo"]

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
          , iacImportRenamingTerms = mempty
          , iacDepsTerms = mempty
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
          , iacImportRenamingTerms = mempty
          , iacDepsTerms = mempty
          }
  importFromApp p iac
  let iac' =
        IAC
          { iacImportRenamingTypes = Map.fromList [("Rose", ("T", Map.fromList [("MkRose", "C")]))]
          , iacDepsTypes = listRenaming
          , iacImportRenamingTerms = mempty
          , iacDepsTerms = mempty
          }
  importFromApp p iac'
  result <- gets appProg
  expect <- srcProg2 ("L", "Nil", "Cons") ("T", "C")
  return $ result @?= expect

unit_import_rewire_primitive :: Assertion
unit_import_rewire_primitive = runImportTest $ do
  (p, prims, fooId) <- primSrcProg
  let iac =
        IAC
          { iacImportRenamingTypes = Map.fromList [("Char", ("Char", mempty))]
          , iacDepsTypes = mempty
          , iacImportRenamingTerms = Map.fromList [(prims Map.! "toUpper", "UPPER")]
          , iacDepsTerms = mempty
          }
  importFromApp p iac
  upperId <- gets $ defID . unsafeHead . filter ((== "UPPER") . defName) . Map.elems . progDefs . appProg
  let iac' =
        IAC
          { iacImportRenamingTypes =
              Map.fromList
                [ ("Bool", ("B", Map.fromList [("True", "tt"), ("False", "ff")]))
                , ("String", ("S", Map.fromList [("Empty", "Nil"), ("HeadAnd", "Cons")]))
                ]
          , iacDepsTypes = Map.singleton "Char" ("Char", mempty)
          , iacImportRenamingTerms = Map.fromList [(prims Map.! "eqChar", "=="), (fooId, "foo")]
          , iacDepsTerms = Map.singleton (prims Map.! "toUpper") upperId
          }
  importFromApp p iac'
  result <- gets appProg
  return $ do
    map typeDefName (progTypes result) @?= ["Char", "B", "S"]
    assertBool "There are holes in the resultant program" $
      all holeFree (Map.elems $ progDefs result)
    defName <$> Map.elems (progDefs result) @?= ["UPPER", "==", "foo"]

-- cannot import without deps
unit_import_ref_not_handled :: Assertion
unit_import_ref_not_handled = do
  runImportTestError (ReferencedTypeNotHandled "List") $ do
    p <- srcProg2 ("List", "Nil", "Cons") ("Rose", "MkRose")
    let iac =
          IAC
            { iacImportRenamingTypes = Map.fromList [("Rose", ("T", Map.fromList [("MkRose", "C")]))]
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (ReferencedTypeNotHandled "Nat") $ do
    (p, plusId, _) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = Map.singleton plusId "plus"
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  -- The '3' here is _plusId, which is hardcoded as it is not in scope
  -- This may be fragile!
  runImportTestError (ReferencedGlobalNotHandled 3) $ do
    (p, _plusId, multId) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Nat" ("Nat", Map.fromList [("Zero", "Zero"), ("Succ", "Succ")])
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = Map.singleton multId "mult"
            , iacDepsTerms = mempty
            }
    importFromApp p iac

-- cannot claim to support deps with free name
unit_import_rewire_tgt_exist :: Assertion
unit_import_rewire_tgt_exist = do
  runImportTestError (UnknownRewrittenTgtType "L") $ do
    p <- srcProg2 ("List", "Nil", "Cons") ("Rose", "MkRose")
    let iac =
          IAC
            { iacImportRenamingTypes = Map.fromList [("Rose", ("T", Map.fromList [("MkRose", "C")]))]
            , iacDepsTypes = Map.singleton "List" ("L", mempty)
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (UnknownRewrittenTgtTerm 0) $ do
    (p, plusId, multId) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Nat" ("N", Map.fromList [("Zero", "Z"), ("Succ", "S")])
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = Map.singleton multId "mult"
            , iacDepsTerms = Map.singleton plusId 0
            }
    importFromApp p iac

-- cannot claim to support deps with wrong kind
unit_import_rewire_kind_match :: Assertion
unit_import_rewire_kind_match = do
  runImportTestError (RewriteTypeKindMismatch "List" "Unit") $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef unitDef]
    p <- srcProg2 ("List", "Nil", "Cons") ("Rose", "MkRose")
    let iac =
          IAC
            { iacImportRenamingTypes = Map.fromList [("Rose", ("T", Map.fromList [("MkRose", "C")]))]
            , iacDepsTypes = Map.singleton "List" ("Unit", mempty)
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  -- 3 is plusId, 43 is fooId. Hardcoded as not in scope. These may be fragile
  runImportTestError (RewriteTermTypeMismatch 3 43) $ do
    (p, plusId, multId) <- srcProg
    _ <- handleMutationRequest $ Edit [CreateDef $ Just "foo"]
    fooId <- gets $ defID . unsafeHead . filter ((== "foo") . defName) . Map.elems . progDefs . appProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Nat" ("N", Map.fromList [("Zero", "Z"), ("Succ", "S")])
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = Map.singleton multId "times"
            , iacDepsTerms = Map.singleton plusId fooId
            }
    importFromApp p iac

-- cannot import two types/ctors/terms with the same name, or one the same as an existing one
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
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (DuplicateTypes ["Unit"]) $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef unitDef]
    p <- p'
    let iac =
          IAC
            { iacImportRenamingTypes = Map.fromList [("T", ("Unit", Map.fromList [("A", "A"), ("B", "B")]))]
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (DuplicateCtors ["C"]) $ do
    p <- p'
    let iac =
          IAC
            { iacImportRenamingTypes = Map.fromList [("T", ("S", Map.fromList [("A", "C"), ("B", "C")]))]
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (DuplicateCtors ["MkUnit"]) $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef unitDef]
    p <- p'
    let iac =
          IAC
            { iacImportRenamingTypes = Map.fromList [("T", ("S", Map.fromList [("A", "A"), ("B", "MkUnit")]))]
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (DuplicateTerm ["f"]) $ do
    (p, plusId, multId) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Nat" ("Nat", Map.fromList [("Zero", "Zero"), ("Succ", "Succ")])
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = Map.fromList [(plusId, "f"), (multId, "f")]
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (DuplicateTerm ["f"]) $ do
    _ <- handleEditRequest [CreateDef $ Just "f"]
    (p, plusId, _) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Nat" ("Nat", Map.fromList [("Zero", "Zero"), ("Succ", "Succ")])
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = Map.fromList [(plusId, "f")]
            , iacDepsTerms = mempty
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
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp
      newEmptyProg
        { progTypes = [TypeDefAST unitDef]
        , progDefs = mempty
        }
      iac
  runImportTestError (RewriteCtorSrcNotExist "Nat" "Nonexistent") $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef natDef]
    (p, _, _) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Nat" ("Nat", Map.fromList [("Zero", "Zero"), ("Succ", "Succ"), ("Nonexistent", "Nonexistent")])
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (RewriteCtorTgtNotExist "Unit" "Succ") $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef unitDef]
    (p, _, _) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Nat" ("Unit", Map.fromList [("Zero", "MkUnit"), ("Succ", "Succ")])
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (DuplicateRewriteCtor "Nat" ["MkUnit"]) $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef unitDef]
    (p, _, _) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Nat" ("Unit", Map.fromList [("Zero", "MkUnit"), ("Succ", "MkUnit")])
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (RewrittenCtorTypeDiffers "Nat" "Succ" "Nat" "Zero") $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef natDef]
    (p, _, _) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Nat" ("Nat", Map.fromList [("Zero", "Succ"), ("Succ", "Zero")])
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac

-- cannot both import and rename the same type/term
unit_import_import_rename_clash :: Assertion
unit_import_import_rename_clash = do
  runImportTestError (ImportRenameType "Nat") $ do
    (p, _, _) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Nat" ("Nat", Map.fromList [("Zero", "Zero"), ("Succ", "Succ")])
            , iacDepsTypes = Map.singleton "Nat" ("Nat", Map.fromList [("Zero", "Zero"), ("Succ", "Succ")])
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (ImportRenameTerm "f") $ do
    _ <- handleEditRequest [CreateDef Nothing]
    fId <- gets $ fst . Map.findMin . progDefs . appProg
    (p, plusId, _) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Nat" ("Nat", Map.fromList [("Zero", "Zero"), ("Succ", "Succ")])
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = Map.singleton plusId "f"
            , iacDepsTerms = Map.singleton plusId fId
            }
    importFromApp p iac

-- cannot import/rewrite nonexistent type/ctor/term, and have to import/rewrite all ctors of a type
unit_import_nonexist :: Assertion
unit_import_nonexist = do
  runImportTestError (UnknownImportedType "Nonexistent") $ do
    (p, _, _) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Nonexistent" ("T", mempty)
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (UnknownImportedCtor "Nat" "Nonexistent") $ do
    (p, _, _) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Nat" ("Nat", Map.fromList [("Zero", "Zero"), ("Succ", "Succ"), ("Nonexistent", "C")])
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (MissedImportCtor "Nat" "Succ") $ do
    (p, _, _) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Nat" ("Nat", Map.fromList [("Zero", "Zero")])
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (UnknownRewrittenSrcType "Nonexistent") $ do
    (p, _, _) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Nonexistent" ("T", mempty)
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (RewriteCtorSrcNotExist "Nat" "Nonexistent") $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef natDef]
    (p, _, _) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Nat" ("Nat", Map.fromList [("Zero", "Zero"), ("Succ", "Succ"), ("Nonexistent", "C")])
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (RewriteCtorTgtNotExist "Unit" "Nonexistent") $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef unitDef]
    (p, _, _) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Nat" ("Unit", Map.fromList [("Zero", "MkUnit"), ("Succ", "Nonexistent")])
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (MissedRewriteCtor "Nat" "Succ") $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef unitDef]
    (p, _, _) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Nat" ("Unit", Map.fromList [("Zero", "MkUnit")])
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (UnknownImportedTerm 0) $ do
    (p, _, _) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = Map.singleton 0 "foo"
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (UnknownRewrittenSrcTerm 0) $ do
    (p, _, _) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = Map.singleton 0 0
            }
    importFromApp p iac

-- Cannot rewrite a ctor to ctor of different type
-- this will hit "unknown ctor" error
unit_import_rewire_cross_type :: Assertion
unit_import_rewire_cross_type = do
  runImportTestError (RewriteCtorTgtNotExist "Unit" "Succ") $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef unitDef]
    (p, _, _) <- srcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Nat" ("Unit", Map.fromList [("Zero", "MkUnit"), ("Succ", "Succ")])
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac

-- Cannot rename primitive types (either in import or rewire)
unit_import_rename_prim_type :: Assertion
unit_import_rename_prim_type = do
  runImportTestError (CannotRenamePrimitiveType "Char") $ do
    (p, _, _) <- primSrcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Char" ("C", mempty)
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (CannotRenamePrimitiveType "Char") $ do
    (p, _, _) <- primSrcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Int" ("Int", mempty)
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
    let iac' =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Char" ("Int", mempty)
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac'

unit_import_prim_type_ctor :: Assertion
unit_import_prim_type_ctor = do
  runImportTestError (PrimitiveTypeHasNoCtor "Char") $ do
    (p, _, _) <- primSrcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Char" ("Char", Map.singleton "a" "a")
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (PrimitiveTypeHasNoCtor "Char") $ do
    (p, _, _) <- primSrcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Char" ("Char", Map.singleton "a" "a")
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
    let iac' =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Char" ("Char", Map.singleton "a" "a")
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac'

-- Cannot rewire a primitive type to a user-defined type of the same name (or vv)
unit_import_prim_user_type :: Assertion
unit_import_prim_user_type = do
  runImportTestError (RewriteTypePrimitiveMismatch "Char" "Char") $ do
    _ <- handleMutationRequest $ Edit [AddTypeDef unitDef{astTypeDefName = "Char"}]
    (p, _, _) <- primSrcProg
    let iac =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Char" ("Char", mempty)
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
  runImportTestError (RewriteTypePrimitiveMismatch "Char" "Char") $ do
    (p, _, _) <- primSrcProg
    let iac =
          IAC
            { iacImportRenamingTypes = Map.singleton "Char" ("Char", mempty)
            , iacDepsTypes = mempty
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
            }
    importFromApp p iac
    let iac' =
          IAC
            { iacImportRenamingTypes = mempty
            , iacDepsTypes = Map.singleton "Char" ("Char", mempty)
            , iacImportRenamingTerms = mempty
            , iacDepsTerms = mempty
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
