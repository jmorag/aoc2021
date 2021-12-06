{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module AOCUtils where

import Data.Attoparsec.ByteString.Char8
import Data.FileEmbed
import Debug.Trace
import Language.Haskell.TH
import Relude hiding (traceShow)
import System.Directory

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

embedInput :: DecsQ
embedInput = do
  thisMod <- loc_module <$> location
  files <- runIO $ listDirectory ("data/" <> thisMod)
  forM (filter (not . isPrefixOf ".") files) \file -> do
    let var = varP (mkName file)
        embed = embedFile =<< makeRelativeToProject ("data/" <> thisMod <> "/" <> file)
    valD var (normalB embed) []
