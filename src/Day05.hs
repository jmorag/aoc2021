{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Day05 where

import AOCUtils
import Data.Attoparsec.ByteString.Char8
import Data.Ix
import Data.Map.Strict qualified as M
import Relude
import Test.Hspec

data Segment = Segment {x1, y1, x2, y2 :: Int}
  deriving (Show)

embedInput

p = r $ many do
  x1 <- decimal
  ","
  y1 <- decimal
  " -> "
  x2 <- decimal
  ","
  y2 <- decimal
  "\n"
  pure $ Segment {x1, y1, x2, y2}

line :: Bool -> Segment -> [(Int, Int)]
line diagonals s@Segment {..}
  | x1 == x2 || y1 == y2 = range (min p1 p2, max p1 p2)
  | diagonals && abs rise == abs run = mkDiag p1 p2
  | otherwise = []
  where
    p1 = (x1, y1)
    p2 = (x2, y2)
    (rise, run) = slope s
    mkDiag p1@(x1, y1) p2
      | p1 == p2 = [p2]
      | otherwise = p1 : mkDiag (x1 + signum run, y1 + signum rise) p2

slope Segment {..} = (y2 - y1, x2 - x1)

type Cover = Map (Int, Int) Int

mkCover :: Bool -> [Segment] -> Cover
mkCover diagonals = foldl' go mempty
  where
    go :: Cover -> Segment -> Cover
    go acc segment = foldr (\pt -> M.insertWith (+) pt 1) acc (line diagonals segment)

findSafe diagonals segments = mkCover diagonals segments & M.filter (>= 2) & M.size

spec = hspec do
  describe "part1" do
    it "should calculate the number of points covered by at least two horizontal or vertical lines" do
      findSafe False (p test1) `shouldBe` 5
  describe "part2" do
    it "should calculate the number of points covered by at least two lines" do
      findSafe True (p test1) `shouldBe` 12

part1 = findSafe False (p input)

part2 = findSafe True (p input)
