{-# LANGUAGE TemplateHaskell #-}

module Day09 where

import AOCUtils
import Data.Array
import qualified Data.ByteString.Char8 as B
import Data.Char
import qualified Data.Set as S

embedInput

type Grid = Array (Int, Int) Int

p str = case B.lines str of
  [] -> error "empty array"
  rows@(row : _) ->
    listArray
      ((1, 1), (length rows, B.length row))
      (map digitToInt $ concatMap B.unpack rows)

neighbors (x, y) grid =
  filter (inRange (bounds grid)) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

isLow (x, y) grid =
  all (\ix -> grid ! ix > grid ! (x, y)) (neighbors (x, y) grid)

allLow grid = filter (`isLow` grid) (indices grid)

riskLevel grid = map ((+ 1) . (grid !)) (allLow grid) & sum

spec = hspec do
  describe "part 1" do
    it "should calculate the risk level" do
      riskLevel (p test1) `shouldBe` 15
  describe "part 2" do
    it "should find 4 basins" do
      S.size (allBasins (p test1)) `shouldBe` 4
    it "should find the sizes of the 4 basins" do
      (allBasins (p test1) & toList & map S.size & sort) `shouldBe` [3, 9, 9, 14]
    it "should find the sizes of the 3 largest basins and multiply them" do
      p2 (p test1) `shouldBe` 1134

part1 = riskLevel (p input)

findBasin grid = go mempty
  where
    go seen ix
      | grid ! ix == 9 = mempty
      | otherwise =
        S.insert ix $
          S.unions $
            map
              (go (S.insert ix seen))
              (neighbors ix grid & filter (`S.notMember` seen))

allBasins grid = go (fromList $ indices grid) where
  go ixSet = case S.minView ixSet of
    Nothing -> mempty
    Just (ix, ixSet') -> let basin = findBasin grid ix in
      (if S.null basin then id else S.insert basin) (go (ixSet' S.\\ basin))

p2 grid = allBasins grid & toList & map S.size & sort & reverse & take 3 & product

part2 = p2 (p input)
