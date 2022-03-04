{-# LANGUAGE NamedFieldPuns #-}

module TestUtils (
  (@?=),
  assertException,
  insertSessionRow,
  testApp,
  withDbSetup,
) where

import Foreword hiding (try)

import Control.Monad.Catch (MonadCatch, try)
import Control.Monad.Fresh (MonadFresh (..))
import qualified Data.Map.Strict as Map
import Data.String (String)
import Data.Text (unpack)
import Data.Typeable (typeOf)
import qualified Database.PostgreSQL.Simple.Options as Options
import Database.Postgres.Temp (
  DB,
  DirectoryType (Temporary),
  cacheAction,
  cacheConfig,
  cacheDirectoryType,
  cacheTemporaryDirectory,
  defaultCacheConfig,
  optionsToDefaultConfig,
  toConnectionString,
  withConfig,
  withDbCacheConfig,
 )
import GHC.Err (error)
import Hasql.Connection (
  Connection,
  acquire,
  release,
 )
import Hasql.Session (run, statement)
import Network.Socket.Free (getFreePort)
import Primer.App (
  App (..),
  InitialApp (NewApp),
  Prog (..),
  defaultTypeDefs,
  newEmptyApp,
  newEmptyProg,
 )
import Primer.Core (
  ASTDef (..),
  Def (DefAST, DefPrim),
  ID,
  Kind (KType),
  PrimDef (..),
  PrimFun,
  defID,
  primDefType,
  primFunType,
 )
import Primer.Core.DSL (
  aPP,
  ann,
  app,
  branch,
  case_,
  con,
  create,
  emptyHole,
  global,
  hole,
  lAM,
  lam,
  letType,
  let_,
  letrec,
  tEmptyHole,
  tapp,
  tcon,
  tforall,
  tfun,
  thole,
  tvar,
  var,
 )
import Primer.Database.Rel8.Schema as Schema hiding (app)
import Primer.Name (Name)
import Primer.Primitives (
  allPrimDefs,
 )
import Rel8 (
  Expr,
  Insert (Insert, into, onConflict, returning, rows),
  OnConflict (Abort),
  Returning (NumberOfRowsAffected),
  insert,
  values,
 )
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (
  proc,
  runProcess_,
 )
import Test.Tasty.HUnit (
  assertBool,
  assertFailure,
 )
import qualified Test.Tasty.HUnit as HUnit

-- The PostgreSQL host, username, and password can be chosen
-- statically, but we need to choose the port dynamically in order to
-- accommodate multiple simultaneous PostgreSQL instances.

host :: String
host = "localhost"

user :: String
user = "postgres"

password :: String
password = "primer"

connectDb :: DB -> IO Connection
connectDb =
  acquire . toConnectionString >=> either (maybe empty (error . unpack . decodeUtf8)) pure

-- | This action requires that the Sqitch script @primer-sqitch@ is in
-- the process's path. If you run this test via Nix, Nix will
-- guarantee that precondition.
deployDb :: Int -> DB -> IO ()
deployDb port _ =
  let url = "db:postgres://" <> user <> ":" <> password <> "@" <> host <> ":" <> show port
   in runProcess_ $ proc "primer-sqitch" ["deploy", "--verify", url]

withDbSetup :: (Connection -> IO ()) -> IO ()
withDbSetup f = do
  -- NOTE: there's a race where the returned port could be opened by
  -- another process before we can use it, but it's extremely unlikely
  -- to be triggered.
  port <- getFreePort
  let throwEither x = either throwIO pure =<< x
      dbConfig =
        optionsToDefaultConfig
          mempty
            { Options.port = pure port
            , Options.user = pure user
            , Options.password = pure password
            , Options.host = pure host
            }
  throwEither $
    withSystemTempDirectory "primer-tmp-postgres" $ \tmpdir ->
      let cc =
            defaultCacheConfig
              { cacheTemporaryDirectory = tmpdir
              , cacheDirectoryType = Temporary
              }
       in withDbCacheConfig cc $ \dbCache ->
            let combinedConfig = dbConfig <> cacheConfig dbCache
             in do
                  migratedConfig <- throwEither $ cacheAction (tmpdir <> "/primer-rel8") (deployDb port) combinedConfig
                  withConfig migratedConfig $ \db ->
                    bracket (connectDb db) release f

(@?=) :: (MonadIO m, Eq a, Show a) => a -> a -> m ()
x @?= y = liftIO $ x HUnit.@?= y
infix 1 @?=

type ExceptionPredicate e = (e -> Bool)

assertException ::
  (HasCallStack, Exception e, MonadIO m, MonadCatch m) =>
  String ->
  ExceptionPredicate e ->
  m a ->
  m ()
assertException msg p action = do
  r <- try action
  case r of
    Right _ -> liftIO $ assertFailure $ msg <> " should have thrown " <> exceptionType <> ", but it succeeded"
    Left e -> liftIO $ assertBool (wrongException e) (p e)
  where
    wrongException e = msg <> " threw " <> show e <> ", but we expected " <> exceptionType
    exceptionType = (show . typeOf) p

-- | Like @MonadDb.insertSession@, but allows us to insert things
-- directly into the database that otherwise might not be permitted by
-- the type system. This is useful for testing purposes.
insertSessionRow :: Schema.SessionRow Expr -> Connection -> IO ()
insertSessionRow row conn =
  void $
    flip run conn $
      statement () $
        insert
          Insert
            { into = Schema.sessionRowSchema
            , rows =
                values
                  [ row
                  ]
            , onConflict = Abort
            , returning = NumberOfRowsAffected
            }

-- | This definition contains every construct in the Primer language.
--
-- TODO: this is identical to a program in the core Primer test suite,
-- so it should be refactored into a common test library. See:
-- https://github.com/hackworthltd/primer/issues/273
testASTDef :: ASTDef
testASTDef =
  ASTDef
    { astDefName = "1"
    , astDefID
    , astDefExpr
    , astDefType
    }
  where
    ((astDefExpr, astDefType), astDefID) = create $ (,) <$> e <*> t
    t =
      tfun
        (tcon "Nat")
        ( tforall
            "a"
            KType
            ( tapp
                ( thole
                    ( tapp
                        (tcon "List")
                        tEmptyHole
                    )
                )
                (tvar "a")
            )
        )
    e =
      let_
        "x"
        (con "True")
        ( letrec
            "y"
            ( app
                ( hole
                    (con "Just")
                )
                ( hole
                    (global 0)
                )
            )
            ( thole
                (tcon "Maybe")
            )
            ( ann
                ( lam
                    "i"
                    ( lAM
                        "β"
                        ( app
                            ( aPP
                                ( letType
                                    "b"
                                    (tcon "Bool")
                                    ( aPP
                                        (con "Left")
                                        (tvar "b")
                                    )
                                )
                                (tvar "β")
                            )
                            ( case_
                                (var "i")
                                [ branch
                                    "Zero"
                                    []
                                    (con "False")
                                , branch
                                    "Succ"
                                    [
                                      ( "n"
                                      , Nothing
                                      )
                                    ]
                                    ( app
                                        ( app
                                            emptyHole
                                            (var "x")
                                        )
                                        (var "y")
                                    )
                                ]
                            )
                        )
                    )
                )
                ( tfun
                    (tcon "Nat")
                    ( tforall
                        "α"
                        KType
                        ( tapp
                            ( tapp
                                (tcon "Either")
                                (tcon "Bool")
                            )
                            (tvar "α")
                        )
                    )
                )
            )
        )

-- | Helper function for creating test apps from a predefined list of
-- 'ASTDef's and 'PrimFun's.
--
-- TODO: move this function into 'Primer.App'. See:
-- https://github.com/hackworthltd/primer/issues/273#issuecomment-1058713380
mkTestDefs :: [ASTDef] -> Map Name PrimFun -> (Map ID Def, ID)
mkTestDefs astDefs primMap =
  let (defs, nextID) = create $ do
        primDefs <- for (Map.toList primMap) $ \(primDefName, def) -> do
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

-- | An initial test 'App' instance that contains all default type
-- definitions (including primitive types), all primitive functions,
-- and a top-level definition that contains every construct in the
-- Primer language.x
testApp :: App
testApp =
  newEmptyApp
    { appProg = testProg
    , appInit = NewApp
    , appIdCounter = fromEnum nextId
    }
  where
    (defs, nextId) = mkTestDefs [testASTDef] allPrimDefs
    testProg :: Prog
    testProg =
      newEmptyProg
        { progTypes = defaultTypeDefs
        , progDefs = defs
        }
