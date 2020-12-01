module Day1
    ( 
        part1
    ) where

import Control.Monad


parseInput :: String -> [Integer]
parseInput = map read . lines

part1 ::  String -> String
part1 input = show $ head $ solve1 (parseInput input) 2020

solve1 :: (Eq b, Num b) => [b] -> b -> [b]
solve1 i test = 
  do x <- i
     y <- i
     guard ((x + y) == test)
     pure (x * y)