{-# LANGUAGE TemplateHaskell #-}

module Day02 where

import AOCUtils hiding (Down)

embedInput

data Position = P {depth :: Int, horizontal :: Int}
  deriving (Show, Eq)

data Direction = Forward | Down | Up
  deriving (Show, Eq)

p :: _ -> [(Direction, Int)]
p = r $ many do
  dir <- Forward <$ "forward" <|> Down <$ "down" <|> Up <$ "up"
  " "
  n <- decimal
  "\n"
  pure (dir, n)

calcPosition :: [(Direction, Int)] -> (Int, Int)
calcPosition = foldl' go (0, 0)
  where
    go (horizontal, depth) (dir, n) = case dir of
      Forward -> (horizontal + n, depth)
      Down -> (horizontal, depth + n)
      Up -> (horizontal, depth - n)

part1 = uncurry (*) (calcPosition (p input))

calcPositionAndAim :: [(Direction, Int)] -> (Int, Int, Int)
calcPositionAndAim = foldl' go (0, 0, 0) where
  go (horizontal, depth, aim) (dir, n) = case dir of
    Down -> (horizontal, depth, aim + n)
    Up -> (horizontal, depth, aim - n)
    Forward -> (horizontal + n, depth + aim * n, aim)

part2 = let (h, d, _) = calcPositionAndAim (p input) in h * d
