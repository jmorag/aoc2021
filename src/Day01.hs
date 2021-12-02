{-# LANGUAGE TemplateHaskell #-}

module Day01 where

import Data.Attoparsec.ByteString.Char8
import Data.FileEmbed
import Relude
import Test.Hspec
import Utils

test1 = $(embedFile "data/Day01/tests/test1")

input = $(embedFile "data/Day01/input")

p :: ByteString -> [Int]
p = r (decimal `sepBy` "\n")

numIncreasing ns@(_ : nss) = zipWith (<) ns nss & filter id & length
numIncreasing [] = 0

part1 = numIncreasing (p input)

slidingWindowNumIncreasing = numIncreasing . sums
  where
    sums (x : y : z : rest) = x + y + z : sums (y : z : rest)
    sums _ = []

part2 = slidingWindowNumIncreasing (p input)

spec :: Spec
spec = do
  describe "Part 1" do
    it "should find the number of increasing entries in the list" do
      numIncreasing (p test1) `shouldBe` 7
  describe "Part 2" do
    it "should find the number of increasing entries in the sliding window of sums" do
      slidingWindowNumIncreasing (p test1) `shouldBe` 5
