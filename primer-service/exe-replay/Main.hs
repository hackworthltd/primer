{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Foreword

import Benchmarks (hashProg)
import Control.Arrow (left)
import Control.Monad.Trans.Accum (Accum, AccumT, add, execAccumT, looks, runAccum)
import Data.Coerce (coerce)
import Data.Either.Extra (eitherToMaybe)
import Data.Map qualified as M
import Data.Map.Merge.Strict qualified as MMerge
import Data.Map.Monoidal.Strict qualified as MMon
import Data.String (String)
import Data.Text qualified as T
import Network.HTTP.Client.TLS (newTlsManager)
import Options.Applicative (
  Parser,
  argument,
  eitherReader,
  execParser,
  flag',
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  str,
 )
import Primer.API (
  APILog (
    ActionOptions,
    ApplyActionInput,
    ApplyActionNoInput,
    AvailableActions,
    GetProgram',
    ListSessions
  ),
  NewSessionReq,
  ReqResp (Req, Resp),
 )
import Primer.API qualified as API
import Primer.App (Prog)
import Primer.Client (
  actionOptionsOpenAPI,
  applyActionInputOpenAPI,
  applyActionNoInputOpenAPI,
  availableActionsOpenAPI,
  createSession,
  defaultAPIPath,
  getProgram,
  getProgramOpenApi,
 )
import Primer.Database (SessionId)
import Primer.Server (ServantLog (RequestStart))
import Servant.Client (
  BaseUrl (..),
  ClientM,
  Scheme (Http),
  mkClientEnv,
  parseBaseUrl,
  runClientM,
 )
import System.Environment (lookupEnv)
import Prelude (error)

-- | The default URL assumes you're running the service on localhost.
defaultBaseUrl :: BaseUrl
defaultBaseUrl =
  BaseUrl
    { baseUrlScheme = Http
    , baseUrlHost = "localhost"
    , baseUrlPort = 8081
    , baseUrlPath = defaultAPIPath
    }

data ReplayOptions = ReplayOptions
  { server :: Maybe BaseUrl
  , replayLog :: FilePath
  }

data Options
  = MkFixture
  | Replay ReplayOptions

getReplayOptions :: Parser ReplayOptions
getReplayOptions =
  ReplayOptions
    <$> optional (option (eitherReader (left show . parseBaseUrl)) (long "base-url"))
    <*> argument str (metavar "FILE-TO-REPLAY")

getOptions :: Parser Options
getOptions =
  flag'
    MkFixture
    ( long "mkfixture"
        <> help "Read a 'primer --record-replay' log on stdin and create a fixture on stdout"
    )
    <|> Replay <$> getReplayOptions

baseUrlEnvVar :: String
baseUrlEnvVar = "PRIMER_URL"

-- | When no base URL is provided on the command line, we try first to
-- lookup and parse the @PRIMER_URL@ environment variable. If that's
-- not present, we fall back to a default.
noBaseUrlOption :: IO BaseUrl
noBaseUrlOption = do
  envVar <- lookupEnv baseUrlEnvVar
  case envVar of
    Nothing -> pure defaultBaseUrl
    Just val -> parseBaseUrl val

expectedResultPrefix :: Text
expectedResultPrefix = "expected result hash: "

{-
Note [Formats]
There are two relevant formats: the input for `--mkfixture` and the output from `--mkfixture`
(which is also the input to the replay mode, i.e. without `--mkfixture`).

Input format
The input format is simply a log from a server running in `--record-replay`
mode. Note that we assume that the block between 'RequestStart's are exactly the
API calls of one request. This is technically not guaranteed -- the onus is on
the user to provide only log files obeying this invariant. However, it is
expected that these logs will be captured on a local dev machine, in which case
it is likely that this invariant is obeyed, since there will not be concurrent
requests. (If we had unique traces attached to requests we would be able to do
better here!) Note that a log containing interaction with multiple sessions is
supported.

Transformation
`--mkfixture` will record
- each initial API call (i.e. the "user-initiated" ones, rather than internal
  ones which happen to be logged also).
- the (hash of the) result of any edits, so that the replay mode can sanity
  check its results.

Output format
The output comes in two sections. Firstly a header of multiple lines of the
format
  expected result hash: (<recordedSessionId>, <hashOfFinalProgram>)
where the <recordedSessionId>s should be unique.
Secondly, a record of API calls. This is multiple lines from the 'Show' instance
of 'ReplayLog' (note the invariants on that type).
-}
run :: Options -> IO ()
run MkFixture = do
  ls <- lines <$> getContents
  let getReplayLog = T.stripPrefix "[REPLAY] "
  let getReqStart = fmap Left . (\case RequestStart -> Just ()) <=< readMaybe @ServantLog
  let getAPILog = fmap Right . readMaybe @APILog
  -- 'drop 1' as don't want the initial segment before the first 'RequestStart' (which should be nil)
  let logs = drop 1 $ splitOnNothing eitherToMaybe $ mapMaybe ((getReqStart <> getAPILog) <=< getReplayLog) ls
  let mkReplay :: [APILog] -> Maybe (Accum (MMon.MonoidalMap SessionId (Last Prog)) ReplayLog)
      mkReplay = \case
        (API.NewSession (Req r) : rest)
          | Just (API.NewSession (Resp sid)) <- lastMay rest -> Just $ do
              whenM (looks $ MMon.member sid) $ error $ "duplicated NewSession IDs: " <> show sid
              addEntry sid $ Last Nothing
              pure $ NewSession r sid
          | otherwise -> error "a 'NewSession Req' did not have a matching 'NewSession Resp'"
        -- This API call is not specific to a session, ignore it
        (ListSessions{} : _) -> Nothing
        (r : rest) -> Just $ do
          -- All changes go via the (internal) 'Edit' API.
          -- 'Accum' their returned 'Prog', to record an expected result
          let lastSuccess' = \case
                [] -> Nothing
                Resp (Right p) : Req (sid, _) : _ -> Just (sid, p)
                Resp (Left _) : Req _ : edits -> lastSuccess' edits
                _ -> error "expected edits to come in pairs"
          let lastSuccess = lastSuccess' . reverse
          case lastSuccess [e | API.Edit e <- r : rest] of
            Nothing -> pure ()
            Just (sid, p) -> addEntry sid $ pure p
          pure $ APILog r
        [] -> error "unexpected: two consecutive RequestStart lines"
  let (replayLog, lastProgs) = runAccum (sequence $ mapMaybe mkReplay logs) mempty
  case getLast $ sequence lastProgs of
    Nothing -> die "Some session did not have any GetProgram"
    Just ps -> do
      void $ MMon.traverseWithKey (\sid p -> putStr expectedResultPrefix >> print (sid, hashProg p)) ps
      mapM_ print replayLog
run (Replay opts) = do
  baseUrl <- maybe noBaseUrlOption pure opts.server
  manager <- newTlsManager
  let env = mkClientEnv manager baseUrl
  (replayLog', expected) <- parseReplay . lines <$> readFile opts.replayLog
  runClientM (replay replayLog') env >>= \case
    Left err -> error $ show err
    Right result -> do
      void $
        MMerge.mergeA
          ( MMerge.mapMissing $ \recordedId _ ->
              error $ "a replayed session has no expected hash, recorded id: " <> show recordedId
          )
          ( MMerge.mapMissing $ \recordedId _ ->
              error $ "recorded a session with no matching replay: " <> show recordedId
          )
          ( MMerge.zipWithMaybeMatched $ \recordedId (actualId, r) e ->
              if hashProg r == e
                then Nothing
                else error $ "Incorrect result for (recordedId,actualId, expectedHash, actualHash): " <> show (recordedId, actualId, e, hashProg r)
          )
          result
          expected

splitOnNothing :: (a -> Maybe b) -> [a] -> [[b]]
splitOnNothing f = go []
  where
    go acc [] = [reverse acc]
    go acc (x : xs)
      | Just b <- f x = go (b : acc) xs
      | otherwise = reverse acc : go [] xs

data ReplayLog
  = NewSession NewSessionReq SessionId
  | -- | Invariant: always a 'Req' (not a 'Resp'), and never a 'NewSession'
    APILog APILog
  deriving stock (Show, Read)

parseReplay :: [Text] -> ([ReplayLog], M.Map SessionId Int)
parseReplay =
  spanMaybe (T.stripPrefix expectedResultPrefix) <&> \(expected, replayLog) ->
    (map unsafeRead replayLog, M.fromList $ map unsafeRead expected)
  where
    -- This may throw an exception, but that will show as a failing test
    unsafeRead :: Read a => Text -> a
    unsafeRead l = fromMaybe (error $ "failed to parse line: " <> toS l) $ readMaybe l

-- | Similar to 'Data.List.span', but takes a 'Maybe' predicate.
spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe f = go
  where
    go [] = ([], [])
    go xxs@(x : xs) = case f x of
      Just b -> first (b :) $ go xs
      Nothing -> ([], xxs)

replay :: [ReplayLog] -> ClientM (M.Map SessionId (SessionId, Prog))
replay l = do
  sidMap <- execAccumT (mapM_ replay' l) mempty
  traverse (\sid -> (sid,) <$> getProgram sid) sidMap
  where
    replay' :: ReplayLog -> AccumT (M.Map SessionId SessionId) ClientM ()
    replay' = \case
      NewSession req recordedId -> do
        actualSId <- lift $ createSession req
        addEntry recordedId actualSId
      APILog l' -> case l' of
        GetProgram' (Req (opts, id)) -> rename_ id $ \id' -> getProgramOpenApi id' opts
        AvailableActions (Req (id, lvl, sel)) -> rename_ id $ \id' -> availableActionsOpenAPI id' lvl sel
        ActionOptions (Req (id, lvl, sel, act)) -> rename_ id $ \id' -> actionOptionsOpenAPI id' lvl sel act
        ApplyActionNoInput (Req (opts, id, sel, act)) -> rename_ id $ \id' -> applyActionNoInputOpenAPI id' opts sel act
        ApplyActionInput (Req (opts, id, bdy, act)) -> rename_ id $ \id' -> applyActionInputOpenAPI id' opts bdy act
        r -> error $ "Unsupported action to replay: " ++ show r
    rename :: Monad m => SessionId -> (SessionId -> m a) -> AccumT (M.Map SessionId SessionId) m a
    rename recordedId f = do
      mb <- looks (M.!? recordedId)
      case mb of
        Nothing -> error $ "Unknown recorded session: " <> show recordedId
        Just actualId -> lift $ f actualId
    rename_ :: Monad m => SessionId -> (SessionId -> m a) -> AccumT (M.Map SessionId SessionId) m ()
    rename_ = fmap (fmap void) rename

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info
        (helper <*> getOptions)
        ( fullDesc
            <> progDesc "Replay a Primer session."
            <> header
              "primer-replay - A replay engine for the Primer API."
        )

addEntry :: (Monad m, Coercible map M.Map) => k -> a -> AccumT (map k a) m ()
addEntry k v = add $ coerce $ M.singleton k v
