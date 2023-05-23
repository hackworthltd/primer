{-# LANGUAGE OverloadedStrings #-}

module Primer.Database.Selda.Test.Util (
  deployDb,
  withDbSetup,
  lowPrecisionCurrentTime,
  runTmpDb,
  insertSessionRow,
  mkSessionRow,
  mkSessionRow',
) where

import Foreword

import Control.Monad.Log (
  DiscardLoggingT,
  WithSeverity,
  discardLogging,
 )
import Data.Aeson qualified as Aeson (
  encode,
 )
import Data.Time (
  UTCTime (..),
  diffTimeToPicoseconds,
  picosecondsToDiffTime,
 )
import Data.UUID.V4 (nextRandom)
import Database.Selda (
  insert_,
 )
import Primer.App (newApp)
import Primer.Database (
  LastModified (..),
  getCurrentTime,
 )
import Primer.Database.Selda.SQLite (
  SeldaSQLiteDbT,
  SessionRow (..),
  runSeldaSQLiteDbT,
  sessions,
 )
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (
  proc,
  runProcess_,
 )

-- | This action requires that the Sqitch script @primer-sqitch@ is in
-- the process's path. If you run this test via Nix, Nix will
-- guarantee that precondition.
deployDb :: FilePath -> IO ()
deployDb db =
  let url = "db:sqlite:" <> db
   in runProcess_ $ proc "primer-sqitch" ["deploy", "--verify", url]

withDbSetup :: (FilePath -> IO ()) -> IO ()
withDbSetup f = do
  -- Note: don't use "System.IO.Temp.withSystemTempFile" here:
  -- @sqitch@ will create additional @.db@ files to manage its own
  -- schema, so we need to use a temp directory and have it all
  -- cleaned up properly.
  withSystemTempDirectory "primer-tmp-sqlite-" $ \dirName -> do
    let db = dirName <> "/primer.db"
    deployDb db
    f db

runTmpDb :: SeldaSQLiteDbT (DiscardLoggingT (WithSeverity ()) IO) () -> IO ()
runTmpDb tests =
  withDbSetup $ \db -> discardLogging $ runSeldaSQLiteDbT db tests

-- | In order to compare times for our tests, we need to truncate the
-- precision of the time returned by 'getCurrentTime'.
lowPrecisionCurrentTime :: (MonadIO m) => m LastModified
lowPrecisionCurrentTime = do
  LastModified (UTCTime day time) <- getCurrentTime
  -- truncate to microseconds
  let time' = picosecondsToDiffTime $ diffTimeToPicoseconds time `div` 1000000 * 1000000
  pure $ LastModified $ UTCTime day time'

-- | Like @MonadDb.insertSession@, but allows us to insert things
-- directly into the database that otherwise might not be permitted by
-- the type system. This is useful for testing purposes.
insertSessionRow :: (MonadIO m, MonadMask m) => SessionRow -> SeldaSQLiteDbT m ()
insertSessionRow row = insert_ sessions [row]

-- | Return a 'SessionRow', which is useful for testing the database
-- without needing to go through the Primer API.
mkSessionRow :: Int -> IO SessionRow
mkSessionRow n = do
  u <- nextRandom
  now <- lowPrecisionCurrentTime
  pure $
    SessionRow
      { uuid = u
      , gitversion = "test-version"
      , app = Aeson.encode newApp
      , name = "name-" <> show n
      , lastmodified = utcTime now
      }

-- | Like 'mkSessionRow', but with a callback to generate names.
mkSessionRow' :: (Int -> Text) -> Int -> IO SessionRow
mkSessionRow' mkName n = do
  u <- nextRandom
  now <- lowPrecisionCurrentTime
  pure $
    SessionRow
      { uuid = u
      , gitversion = "test-version"
      , app = Aeson.encode newApp
      , name = mkName n
      , lastmodified = utcTime now
      }
