module Utils where

import Data.Attoparsec.ByteString.Char8
import Debug.Trace
import Prelude
import Data.FileEmbed (embedStringFile)

r parser string = case feed (parse parser string) mempty of
  Done "" x -> x
  Done trailing x -> traceShow ("leftover: " <> trailing) x
  Fail trailing contexts err ->
    error $
      unlines
        [ "leftover: " <> show trailing
        , "contexts: " <> show contexts
        , "error: " <> show err
        ]
  Partial _ -> error "impossible"
