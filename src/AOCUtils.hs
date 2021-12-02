{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module AOCUtils where

import Data.Attoparsec.ByteString.Char8
import Data.FileEmbed
import Debug.Trace
import Language.Haskell.TH
import Relude (ByteString)
import Prelude

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

embed path = embedFile =<< makeRelativeToProject path

embedInput :: DecsQ
embedInput = do
  thisMod <- loc_module <$> location
  [d|input :: ByteString; input = $(embed ("data/" <> thisMod <> "/input"))|]
