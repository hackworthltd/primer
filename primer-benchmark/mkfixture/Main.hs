module Main (main) where

import Foreword

import Benchmarks (hashProg)
import Data.Bitraversable (bitraverse)
import Data.Text qualified as T
import Data.UUID (UUID)
import Primer.App (MutationRequest, Prog)

main :: IO ()
main = do
  ls <- lines <$> getContents
  let getEdit = fmap (Left . T.dropEnd 1) . T.stripPrefix "[REPLAY] Edit (Req "
  let getProg = fmap (Right . T.dropEnd 1) . T.stripPrefix "[REPLAY] GetProgram (Resp "
  let getInteresting = getEdit <> getProg
  let ls' = mapMaybe getInteresting ls
  case traverse (bitraverse (readEither @(UUID, MutationRequest)) (readEither @Prog)) ls' of
    Left err -> die $ "Parse failure: " <> err
    Right umps -> do
      expected <- case unsnoc umps of
        Just (_, Right p) -> pure $ hashProg p
        _ -> die "Did not find a 'GetProgram' response after the last edit"
      let (us, ms) = unzip $ mapMaybe leftToMaybe umps
      case ordNub us of
        [] -> die "Error: no edits found"
        us'@(_ : _ : _) -> die $ "Error: edits found from multiple sessions: " <> show us'
        [_] -> pure ()
      putStr @Text "expected result hash: "
      print expected
      mapM_ print ms
