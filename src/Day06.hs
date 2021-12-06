module Day06 where

import AOCUtils
import Control.Monad.Memo
import Data.List ((!!))

test1 = [3, 4, 3, 1, 2]

input :: [Int]
input = [2, 5, 2, 3, 5, 3, 5, 5, 4, 2, 1, 5, 5, 5, 5, 1, 2, 5, 1, 1, 1, 1, 1, 5, 5, 1, 5, 4, 3, 3, 1, 2, 4, 2, 4, 5, 4, 5, 5, 5, 4, 4, 1, 3, 5, 1, 2, 2, 4, 2, 1, 1, 2, 1, 1, 4, 2, 1, 2, 1, 2, 1, 3, 3, 3, 5, 1, 1, 1, 3, 4, 4, 1, 3, 1, 5, 5, 1, 5, 3, 1, 5, 2, 2, 2, 2, 1, 1, 1, 1, 3, 3, 3, 1, 4, 3, 5, 3, 5, 5, 1, 4, 4, 2, 5, 1, 5, 5, 4, 5, 5, 1, 5, 4, 4, 1, 3, 4, 1, 2, 3, 2, 5, 1, 3, 1, 5, 5, 2, 2, 2, 1, 3, 3, 1, 1, 1, 4, 2, 5, 1, 2, 4, 4, 2, 5, 1, 1, 3, 5, 4, 2, 1, 2, 5, 4, 1, 5, 5, 2, 4, 3, 5, 2, 4, 1, 4, 3, 5, 5, 3, 1, 5, 1, 3, 5, 1, 1, 1, 4, 2, 4, 4, 1, 1, 1, 1, 1, 3, 4, 5, 2, 3, 4, 5, 1, 4, 1, 2, 3, 4, 2, 1, 4, 4, 2, 1, 5, 3, 4, 1, 1, 2, 2, 1, 5, 5, 2, 5, 1, 4, 4, 2, 1, 3, 1, 5, 5, 1, 4, 2, 2, 1, 1, 1, 5, 1, 3, 4, 1, 3, 3, 5, 3, 5, 5, 3, 1, 4, 4, 1, 1, 1, 3, 3, 2, 3, 1, 1, 1, 5, 4, 2, 5, 3, 5, 4, 4, 5, 2, 3, 2, 5, 2, 1, 1, 1, 2, 1, 5, 3, 5, 1, 4, 1, 2, 1, 5, 3, 5, 2, 1, 3, 1, 2, 4, 5, 3, 4, 3]

spec = hspec do
  describe "part 1" do
    it "should count how many fish there are after 18 days" do
      length (simulateNaive 18 test1) `shouldBe` 26
    it "should count how many fish there are after 80 days" do
      length (simulateNaive 80 test1) `shouldBe` 5934
  describe "part 2" do
    it "should count how many fish there are after 18 days" do
      simulateFast 18 test1 `shouldBe` 26
    it "should count how many fish there are after 80 days" do
      simulateFast 80 test1 `shouldBe` 5934
    it "should count how many fish there are after 256 days" do
      simulateFast 256 test1 `shouldBe` 26984457539

simulateNaive :: Int -> [Int] -> [Int]
simulateNaive days =
  (!! days) . iterate (concatMap \case 0 -> [6, 8]; x -> [x - 1])

simulateFast :: Int -> [Int] -> Sum Int
simulateFast days = startEvalMemo . foldMapM (memo2 go days)
  where
    go :: (MonadMemo (Int, Int) (Sum Int) m) => Int -> Int -> m (Sum Int)
    go d f
      | d <= f = pure 1
      | f == 0 = foldMapM (memo2 go (d - 1)) [6, 8]
      | otherwise = memo2 go (d - f) 0

memo2 :: MonadMemo (k1, k2) v m => (k1 -> k2 -> m v) -> k1 -> k2 -> m v
memo2 = for2 memo

part1 = length (simulateNaive 80 input)
part2 = simulateFast 256 input
