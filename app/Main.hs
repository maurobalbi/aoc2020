module Main where

import Day1
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

readInput :: Aoc -> IO String
readInput (Aoc 1 1) = readFile "./resources/day1/part1.txt"
readInput (Aoc 1 2) = readFile "./resources/day1/part2.txt"
readInput _ = return ""

solutions :: Aoc -> IO String

main :: IO ()
main = do
   command <- execParser opts
   string <- readInput command
   putStrLn string
    where
      opts = info (sample <**> helper)
        ( fullDesc
          <> progDesc "Solve a day and part of Advent of Code"
          <> header "Advent of Code 2020 - Solutions" )
