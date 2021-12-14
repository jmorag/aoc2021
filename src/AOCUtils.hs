module AOCUtils (module X, embedInput, r, debugArray) where

import Data.Array
import Data.Attoparsec.ByteString.Char8 as X hiding (take, takeWhile)
import Data.FileEmbed
import Debug.Trace
import Language.Haskell.TH
import Relude hiding (traceShow)
import Relude as X
import System.Directory
import Test.Hspec as X

r parser string = case feed (parse parser string) mempty of
  Done "" x -> x
  Done trailing x -> Debug.Trace.traceShow ("leftover: " <> trailing) x
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
  files <- Language.Haskell.TH.runIO $ listDirectory ("data/" <> thisMod)
  forM (filter (not . isPrefixOf ".") files) \file -> do
    let var = varP (mkName file)
        embed = embedFile =<< makeRelativeToProject ("data/" <> thisMod <> "/" <> file)
    valD var (normalB embed) []

debugArray :: Show a => Array (Int, Int) a -> IO ()
debugArray arr =
  let ((n', m'), (n, m)) = bounds arr
   in forM_ [n' .. n] \i -> do
        forM_ [m' .. m] \j -> putStr (show (arr ! (i, j)))
        putStrLn ""
