{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TestUtils (
  (@?=),
  assertException,
  insertSessionRow,
  runTmpDb,
  testApp,
  withDbSetup,
) where

import Foreword hiding (try)

import Control.Monad.Catch (MonadCatch, try)
import Data.ByteString.Lazy.UTF8 as BL
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
  newEmptyApp,
  newEmptyProg,
 )
import Primer.Builtins (
  builtinModule,
  cFalse,
  cJust,
  cLeft,
  cSucc,
  cTrue,
  cZero,
  tBool,
  tEither,
  tList,
  tMaybe,
  tNat,
 )
import Primer.Core (
  ASTDef (..),
  Def (DefAST),
  GlobalName (baseName),
  ID,
  Kind (KType),
  qualifyName,
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
  gvar',
  hole,
  lAM,
  lam,
  letType,
  let_,
  letrec,
  lvar,
  tEmptyHole,
  tapp,
  tcon,
  tforall,
  tfun,
  thole,
  tvar,
 )
import Primer.Database.Rel8.Rel8Db (
  Rel8Db,
  runRel8Db,
 )
import Primer.Database.Rel8.Schema as Schema hiding (app)
import Primer.Module (
  Module (
    Module,
    moduleDefs,
    moduleName,
    moduleTypes
  ),
 )
import Primer.Primitives (primitiveModule)
import Rel8 (
  Expr,
  Insert (Insert, into, onConflict, returning, rows),
  OnConflict (Abort),
  Returning (NumberOfRowsAffected),
  insert,
  values,
 )
import System.IO.Temp (getCanonicalTemporaryDirectory)
import System.Process.Typed (
  proc,
  readProcessStdout,
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

-- | This action requires that the Sqitch script @primer-sqitch@ is in
-- the process's path. If you run this test via Nix, Nix will
-- guarantee that precondition.
sqitchEventChangeId :: IO String
sqitchEventChangeId = do
  (status, output) <- readProcessStdout $ proc "primer-sqitch" ["plan", "--max-count=1", "--format=format:%h", "--no-headers"]
  case status of
    ExitFailure n -> error $ "`primer-sqitch plan` failed with exit code " <> show n
    _ -> pure $ takeWhile (/= '\n') $ BL.toString output

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
  throwEither $ do
    tmpdir <- getCanonicalTemporaryDirectory
    let cc =
          defaultCacheConfig
            { cacheTemporaryDirectory = tmpdir
            , cacheDirectoryType = Temporary
            }
     in withDbCacheConfig cc $ \dbCache ->
          let combinedConfig = dbConfig <> cacheConfig dbCache
           in do
                hash_ <- sqitchEventChangeId
                migratedConfig <- throwEither $ cacheAction (tmpdir <> "/" <> hash_) (deployDb port) combinedConfig
                withConfig migratedConfig $ \db ->
                  bracket (connectDb db) release f

runTmpDb :: Rel8Db () -> IO ()
runTmpDb tests =
  withDbSetup $ \conn -> runRel8Db tests conn

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
testASTDefNextID :: ID
(testASTDef, testASTDefNextID) =
  ( ASTDef
    { astDefName = qualifyName "TestModule" "1"
    , astDefExpr
    , astDefType
    }
  , nextID
  )
  where
    ((astDefExpr, astDefType), nextID) = create $ (,) <$> e <*> t
    t =
      tfun
        (tcon tNat)
        ( tforall
            "a"
            KType
            ( tapp
                ( thole
                    ( tapp
                        (tcon tList)
                        tEmptyHole
                    )
                )
                (tvar "a")
            )
        )
    e =
      let_
        "x"
        (con cTrue)
        ( letrec
            "y"
            ( app
                ( hole
                    (con cJust)
                )
                ( hole
                    (gvar' "TestModule" "0")
                )
            )
            ( thole
                (tcon tMaybe)
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
                                    (tcon tBool)
                                    ( aPP
                                        (con cLeft)
                                        (tvar "b")
                                    )
                                )
                                (tvar "β")
                            )
                            ( case_
                                (lvar "i")
                                [ branch
                                    cZero
                                    []
                                    (con cFalse)
                                , branch
                                    cSucc
                                    [
                                      ( "n"
                                      , Nothing
                                      )
                                    ]
                                    ( app
                                        ( app
                                            emptyHole
                                            (lvar "x")
                                        )
                                        (lvar "y")
                                    )
                                ]
                            )
                        )
                    )
                )
                ( tfun
                    (tcon tNat)
                    ( tforall
                        "α"
                        KType
                        ( tapp
                            ( tapp
                                (tcon tEither)
                                (tcon tBool)
                            )
                            (tvar "α")
                        )
                    )
                )
            )
        )

-- | An initial test 'App' instance that contains all default type
-- definitions (including primitive types), all primitive functions,
-- and a top-level definition that contains every construct in the
-- Primer language.x
testApp :: App
testApp =
  newEmptyApp
    { appProg = testProg
    , appInit = NewApp
    , appIdCounter = fromEnum testASTDefNextID
    }
  where
    testProg :: Prog
    testProg =
      newEmptyProg
        { progImports = [builtinModule, primitiveModule]
        , progModule =
            Module
              { moduleName = "TestModule"
              , moduleTypes = mempty
              , moduleDefs = Map.singleton (baseName $ astDefName testASTDef) (DefAST testASTDef)
              }
        }
