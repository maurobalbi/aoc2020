{-# LANGUAGE ScopedTypeVariables #-}
module Main where


import qualified Day1(part1, part2)
import qualified Day2(part1, part2)
import qualified Day3(part1, part2)
import qualified Day4(part1, part2)
import qualified Day5(part1, part2)
import qualified Day6(part1, part2)
import Options.Applicative
import Data.Semigroup ()

data Aoc = Aoc
  { day      :: Int
  , part      :: Int }

sample :: Parser Aoc
sample = Aoc
      <$> option auto
          ( long "day"
         <> short 'd'
         <> metavar "INT"
         <> help "The Day to execute" )
      <*>  option auto
          ( long "part"
         <> short 'p'
         <> showDefault
         <> value 1
         <> help "The part to execute"
         <> metavar "INT" )

solve :: Aoc -> IO ()
solve (Aoc 1 1) = readFile  "./input/day1.txt"  >>= putStrLn . Day1.part1
solve (Aoc 1 2) = readFile  "./input/day1.txt"  >>= putStrLn . Day1.part2
solve (Aoc 2 1) = readFile  "./input/day2.txt"  >>= putStrLn . Day2.part1
solve (Aoc 2 2) = readFile  "./input/day2.txt"  >>= putStrLn . Day2.part2
solve (Aoc 3 1) = readFile  "./input/day3.txt"  >>= putStrLn . Day3.part1
solve (Aoc 3 2) = readFile  "./input/day3.txt"  >>= putStrLn . Day3.part2
solve (Aoc 4 1) = readFile  "./input/day4.txt"  >>= putStrLn . Day4.part1
solve (Aoc 4 2) = readFile  "./input/day4.txt"  >>= putStrLn . Day4.part2
solve (Aoc 5 1) = readFile  "./input/day5.txt"  >>= putStrLn . Day5.part1
solve (Aoc 5 2) = readFile  "./input/day5.txt"  >>= putStrLn . Day5.part2
solve (Aoc 6 1) = readFile  "./input/day6.txt"  >>= putStrLn . Day6.part1
solve (Aoc 6 2) = readFile  "./input/day6.txt"  >>= putStrLn . Day6.part2
solve _ = putStrLn "Please specify a correct day and part"

main :: IO ()
main = do
   aoc <- execParser opts
   solve aoc
    where
      opts = info (sample <**> helper)
        ( fullDesc
          <> progDesc "Solve a day and part of Advent of Code"
          <> header "Advent of Code 2020 - Solutions" )
