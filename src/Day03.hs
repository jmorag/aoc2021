{-# LANGUAGE TemplateHaskell #-}

module Day03 where

import AOCUtils
import Data.Attoparsec.ByteString.Char8
import Data.List ((!!))
import Relude
import Test.Hspec

embedInput

type Input = [[Bool]]

p = r $ many do
  many1 (char '0' $> False <|> char '1' $> True) <* "\n"

toDecimal :: [Bool] -> Int
toDecimal = snd . foldr go (0, 0)
  where
    go b (base, acc) = (base + 1, acc + if b then 2 ^ base else 0)

-- | Each bit in the gamma rate can be determined by finding the most common bit
-- in the corresponding position of all numbers in the diagnostic report
gamma :: [[Bool]] -> [Bool]
gamma bbs = map mostCommonBit (transpose bbs)

epsilon = map not . gamma

part1 = toDecimal (gamma (p input)) * toDecimal (epsilon (p input))

mostCommonBit :: [Bool] -> Bool
mostCommonBit bits = foldr go 0 bits >= 0
  where
    go bit acc = acc + if bit then 1 else (-1)

leastCommonBit = not . mostCommonBit

spec = hspec do
  describe "Part 1" do
    it "should calculate the 'gamma' rate of the diagnostic report" do
      toDecimal (gamma (p test1)) `shouldBe` 22
    it "should calculate the 'epsilon' rate of the diagnostic report" do
      toDecimal (epsilon (p test1)) `shouldBe` 9
  describe "Part 2" do
    it "should calculate the 'oxygen generator rating'" do
      toDecimal (oxygen (p test1)) `shouldBe` 23
    it "should calculate the 'CO2 scribber rating'" do
      toDecimal (co2 (p test1)) `shouldBe` 10

findBitCriteria criteria = go 0
  where
    go _ [bs] = bs
    go i bss =
      let mcb = criteria (map (!! i) bss)
       in go (i + 1) (filter (\bs -> (bs !! i) == mcb) bss)

oxygen, co2 :: [[Bool]] -> [Bool]
oxygen = findBitCriteria mostCommonBit
co2 = findBitCriteria leastCommonBit

part2 = toDecimal (oxygen (p input)) * toDecimal (co2 (p input))
