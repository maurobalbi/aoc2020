module Day3
    ( 
        part1,
        part2
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

part2 ::  String -> String
part2 input = show $ head $ solve2 (parseInput input) 2020

solve2 :: (Eq b, Num b) => [b] -> b -> [b]
solve2 i test = 
  do x <- i
     y <- i
     z <- i
     guard ((x + y + z) == test)
     pure (x * y * z)