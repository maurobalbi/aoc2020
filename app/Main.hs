{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Day1
import qualified Day1 as Day1(part1, part2)
import Day2
import qualified Day2 as Day2(part1, part2)
import Day3
import qualified Day3 as Day3(part1, part2)

import Day4
import qualified Day4 as Day4(part1, part2)
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
