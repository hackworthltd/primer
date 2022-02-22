{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

-- This module defines the high level application functions.

module Primer.App.Core (
  Log (..),
  App (..),
  InitialApp (..),
  initialApp,
  newProg,
  newEmptyProg,
  newApp,
  newEmptyApp,
  Prog (..),
  Selection (..),
  NodeSelection (..),
  NodeType (..),
  boolDef,
  natDef,
  listDef,
  eitherDef,
  defaultTypeDefs,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh (..))
import Data.Aeson (
  ToJSON (toEncoding),
  defaultOptions,
  genericToEncoding,
 )
import qualified Data.Map.Strict as Map
import Primer.Action (
  ProgAction (..),
 )

import Primer.Core (
  ASTDef (..),
  ASTTypeDef (..),
  Def (..),
  Expr' (EmptyHole),
  ExprMeta,
  ID (..),
  Kind (..),
  Meta (..),
  PrimDef (..),
  Type' (..),
  TypeDef (..),
  TypeMeta,
  ValCon (..),
  defID,
  primDefID,
  primFunType,
 )
import Primer.Core.DSL (create, emptyHole, tEmptyHole)
import Primer.JSON
import Primer.Name (NameCounter)
import Primer.Primitives (allPrimDefs, allPrimTypeDefs)
import Primer.Typecheck (
  SmartHoles (SmartHoles),
 )

-- | The program state, shared between the frontend and backend
--  This is much more info than we need to send - for example we probably don't
--  need to send the log back and forth.
--  But for now, to keep things simple, that's how it works.
data Prog = Prog
  { progTypes :: [TypeDef]
  , progDefs :: Map ID Def -- The current program: a set of definitions indexed by ID
  , progSelection :: Maybe Selection
  , progSmartHoles :: SmartHoles
  , progLog :: Log -- The log of all actions
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON Prog

-- | The action log
--  This is the canonical store of the program - we can recreate any current or
--  past program state by replaying this log.
--  Each item is a sequence of Core actions which should be applied atomically.
--  Items are stored in reverse order so it's quick to add new ones.
newtype Log = Log {unlog :: [[ProgAction]]}
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON Log

-- | Describes what interface element the user has selected.
-- A definition in the left hand nav bar, and possibly a node in that definition.
data Selection = Selection
  { selectedDef :: ASTDef
  , selectedNode :: Maybe NodeSelection
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via VJSON Selection

-- | A selected node, in the body or type signature of some definition.
-- We have the following invariant: @nodeType = SigNode ==> isRight meta@
data NodeSelection = NodeSelection
  { nodeType :: NodeType
  , nodeId :: ID
  , meta :: Either ExprMeta TypeMeta
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via VJSON NodeSelection

data NodeType = BodyNode | SigNode
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via VJSON NodeType

-- | We use this type to remember which "new app" was used to
-- initialize the session. We need this so that program resets and
-- undo know which baseline app to start with when performing their
-- corresponding action.
data InitialApp
  = NewApp
  | NewEmptyApp
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON InitialApp

-- | Given an 'InitialApp', return the corresponding new app instance.
initialApp :: InitialApp -> App
initialApp NewApp = newApp
initialApp NewEmptyApp = newEmptyApp

-- | The global App state
--
-- Note that the 'ToJSON' and 'FromJSON' instances for this type are
-- not used in the frontend, and therefore we can use "Data.Aeson"s
-- generic instances for them.
data App = App
  { appIdCounter :: Int
  , appNameCounter :: NameCounter
  , appProg :: Prog
  , appInit :: InitialApp
  }
  deriving (Eq, Show, Generic)

instance ToJSON App where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON App

-- | An empty initial program.
newEmptyProg :: Prog
newEmptyProg =
  let expr = EmptyHole (Meta 1 Nothing Nothing)
      ty = TEmptyHole (Meta 2 Nothing Nothing)
      def = DefAST $ ASTDef 0 "main" expr ty
   in Prog
        { progTypes = []
        , progDefs = Map.singleton 0 def
        , progSelection = Nothing
        , progSmartHoles = SmartHoles
        , progLog = Log []
        }

-- | An initial app whose program is completely empty.
newEmptyApp :: App
newEmptyApp =
  App
    { appIdCounter = 3
    , appNameCounter = toEnum 0
    , appProg = newEmptyProg
    , appInit = NewEmptyApp
    }

-- | An initial program with some useful typedefs.
newProg :: Prog
newProg =
  newEmptyProg
    { progTypes = defaultTypeDefs
    , progDefs = defaultDefs
    }

defaultDefsNextId :: ID
defaultDefs :: Map ID Def
(defaultDefs, defaultDefsNextId) =
  let (defs, nextID) = create $ do
        mainExpr <- emptyHole
        mainType <- tEmptyHole
        let astDefs =
              [ ASTDef
                  { astDefID = 0
                  , astDefName = "main"
                  , astDefExpr = mainExpr
                  , astDefType = mainType
                  }
              ]
        primDefs <- for (Map.toList allPrimDefs) $ \(primDefName, def) -> do
          primDefType <- primFunType def
          primDefID <- fresh
          pure $
            PrimDef
              { primDefID
              , primDefName
              , primDefType
              }
        pure $ map DefAST astDefs <> map DefPrim primDefs
   in (Map.fromList $ (\d -> (defID d, d)) <$> defs, nextID)

-- | An initial app whose program includes some useful definitions.
newApp :: App
newApp =
  newEmptyApp
    { appProg = newProg
    , appInit = NewApp
    , appIdCounter = fromEnum defaultDefsNextId
    }

defaultTypeDefs :: [TypeDef]
defaultTypeDefs =
  map
    TypeDefAST
    [boolDef, natDef, listDef, maybeDef, pairDef, eitherDef]
    <> map
      TypeDefPrim
      (Map.elems allPrimTypeDefs)

-- | A definition of the Bool type
boolDef :: ASTTypeDef
boolDef =
  ASTTypeDef
    { astTypeDefName = "Bool"
    , astTypeDefParameters = []
    , astTypeDefConstructors =
        [ ValCon "True" []
        , ValCon "False" []
        ]
    , astTypeDefNameHints = ["p", "q"]
    }

-- | A definition of the Nat type
natDef :: ASTTypeDef
natDef =
  ASTTypeDef
    { astTypeDefName = "Nat"
    , astTypeDefParameters = []
    , astTypeDefConstructors =
        [ ValCon "Zero" []
        , ValCon "Succ" [TCon () "Nat"]
        ]
    , astTypeDefNameHints = ["i", "j", "n", "m"]
    }

-- | A definition of the List type
listDef :: ASTTypeDef
listDef =
  ASTTypeDef
    { astTypeDefName = "List"
    , astTypeDefParameters = [("a", KType)]
    , astTypeDefConstructors =
        [ ValCon "Nil" []
        , ValCon "Cons" [TVar () "a", TApp () (TCon () "List") (TVar () "a")]
        ]
    , astTypeDefNameHints = ["xs", "ys", "zs"]
    }

-- | A definition of the Maybe type
maybeDef :: ASTTypeDef
maybeDef =
  ASTTypeDef
    { astTypeDefName = "Maybe"
    , astTypeDefParameters = [("a", KType)]
    , astTypeDefConstructors =
        [ ValCon "Nothing" []
        , ValCon "Just" [TVar () "a"]
        ]
    , astTypeDefNameHints = ["mx", "my", "mz"]
    }

-- | A definition of the Pair type
pairDef :: ASTTypeDef
pairDef =
  ASTTypeDef
    { astTypeDefName = "Pair"
    , astTypeDefParameters = [("a", KType), ("b", KType)]
    , astTypeDefConstructors = [ValCon "MakePair" [TVar () "a", TVar () "b"]]
    , astTypeDefNameHints = []
    }

-- | A definition of the Either type
eitherDef :: ASTTypeDef
eitherDef =
  ASTTypeDef
    { astTypeDefName = "Either"
    , astTypeDefParameters = [("a", KType), ("b", KType)]
    , astTypeDefConstructors = [ValCon "Left" [TVar () "a"], ValCon "Right" [TVar () "b"]]
    , astTypeDefNameHints = []
    }
