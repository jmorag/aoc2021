{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Day08 where

import AOCUtils
import Control.Lens
import Control.Lens.Unsound
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Prelude qualified as P

{-
--- Day 8: Seven Segment Search ---

Each digit of a seven-segment display is rendered by turning on or off any of seven segments named a through g:

  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg

-}

embedInput

spec = hspec do
  describe "part 1" do
    it "should calculate how many times the digits, 1, 4, 7, and 8 appear" do
      p1 (p test1) `shouldBe` 26
  describe "part 2" do
    it "should calculate the sum of the outputs" do
      p2 (p test1) `shouldBe` 61229

p :: ByteString -> [([String], [String])]
p = r $ many1 do
  l <- word `sepBy` " "
  " | "
  r <- word `sepBy` " "
  "\n"
  pure (l, r)
  where
    word = many1 letter_ascii

p1 :: [([String], [String])] -> Int
p1 = lengthOf (traversed . _2 . traversed . filtered hasUniqueNumSegments)
  where
    hasUniqueNumSegments s = length s `elem` [2, 3, 4, 7]

part1 = p1 (p input)

initialMapping :: Map Char (Set Char)
initialMapping = uniformMapping ['a' .. 'g'] (fromList ['a' .. 'g'])

uniformMapping :: Ord a => [a] -> Set a -> Map a (Set a)
uniformMapping keys val = zip keys (repeat val) & fromList

deduce :: [Set Char] -> Map Char Char
deduce signals =
  let signal1 = findLen 2
      signal7 = findLen 3
      signal4 = findLen 4
      signal8 = findLen 7
      signal235 = findLens 5
      signal069 = findLens 6
   in initialMapping
        & ix 'c' .~ signal1
        & ix 'f' .~ signal1
        & prune "abdeg" signal1
        & ix 'a' .~ (signal7 S.\\ signal1)
        & prune "bdeg" signal7
        & ixs "bd" .~ (signal4 S.\\ signal1)
        & prune "aeg" signal4
        & ixs "adg" %~ (\existing -> foldr S.intersection existing signal235)
        & ixs "abfg" %~ (\existing -> foldr S.intersection existing signal069)
        & reduce1
        & fmap (\set -> if S.size set /= 1 then error "failed deduction" else S.findMax set)
        & M.toList
        & map swap
        & M.fromList
  where
    findLen n = find (\sig -> S.size sig == n) signals ^?! _Just
    findLens n = filter (\sig -> S.size sig == n) signals

prune keys signal m = M.differenceWith (\x y -> Just (x S.\\ y)) m (uniformMapping keys signal)

reduce1 :: Map Char (Set Char) -> Map Char (Set Char)
reduce1 mapping =
  let constrained = M.filter (\s -> S.size s == 1) mapping
   in M.foldrWithKey (\k -> prune (filter (/= k) "abcdefg")) mapping constrained

{- HLINT ignore -}
-- using foldr here interacts poorly with the rank 2 Traversal' type
ixs :: (Ixed m) => [Index m] -> Traversal' m (IxValue m)
ixs [] = \f mapping -> pure mapping
ixs (i : is) = ix i `adjoin` ixs is

decode :: ([String], [String]) -> Int
decode (signals, output) =
  let deduction = deduce (map fromList signals)
      output' = map (map (deduction M.!)) output
   in output' & map (\n -> charsToDigit M.! (fromList n)) & P.read

charsToDigit :: Map (Set Char) Char
charsToDigit =
  fromList $
    zipWith (\chars dig -> (fromList chars, dig))
      ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]
      ['0' .. '9']

p2 :: [([String], [String])] -> Int
p2 = sum . map decode

part2 = p2 (p input)
