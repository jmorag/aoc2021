{-# LANGUAGE TemplateHaskell #-}

module Day04 where

import AOCUtils
import Data.Attoparsec.ByteString.Char8
import Data.List.Split (chunksOf)
import Data.Map.Strict qualified as M
import Relude
import Test.Hspec

embedInput

data Input = Input {drawn :: [Int], boards :: [BoardState]}
  deriving (Show)

p :: ByteString -> Input
p = r do
  drawn <- decimal `sepBy` ","
  skipSpace
  numbers <- decimal `sepBy` skipSpace
  "\n"
  pure $ Input {drawn, boards = map initBoard $ chunksOf 25 numbers}

data BoardState = BoardState {grid :: Map (Int, Int) (Int, Bool), nums :: Map Int (Int, Int)}
  deriving (Show)

initBoard :: [Int] -> BoardState
initBoard b =
  BoardState
    { grid = fromList $ zipWith (\n ix -> (ix, (n, False))) b ixs,
      nums = fromList $ zip b ixs
    }
  where
    ixs = liftA2 (,) [1 .. 5] [1 .. 5]

won :: BoardState -> Bool
won BoardState {grid} = any (all (snd . (grid M.!))) rowCols

rowCols = map (\i -> map (i,) [1 .. 5]) [1 .. 5] <> map (\j -> map (,j) [1 .. 5]) [1 .. 5]

step :: Int -> BoardState -> BoardState
step n bs@BoardState {grid, nums} = case nums M.!? n of
  Just pos -> bs {grid = M.adjust (\(n, _marked) -> (n, True)) pos grid}
  Nothing -> bs

play :: Input -> Int
play Input {drawn, boards} = go (error "turn 1") drawn boards
  where
    go lastDrawn nums@(n : ns) boards =
      case find won boards of
        Nothing -> go n ns (map (step n) boards)
        Just board -> score lastDrawn board

spec = hspec do
  describe "part 1" do
    it "should calculate the score at the end of the bingo game" do
      play (p test1) `shouldBe` 4512
  describe "part 2" do
    it "should calculate the score of the last board to win" do
      playToLose (p test1) `shouldBe` 1924

part1 = play (p input)

score lastDrawn BoardState {grid} = lastDrawn * sum unmarked
  where
    unmarked = M.elems grid & mapMaybe \(n, marked) -> if marked then Nothing else Just n

playToLose :: Input -> Int
playToLose Input {drawn, boards} = go (error "turn 1") drawn boards
  where
    go lastDrawn nums@(n : ns) boards = case boards of
      [board] | won board -> score lastDrawn board
      _ -> go n ns (map (step n) (filter (not . won) boards))

part2 = playToLose (p input)
