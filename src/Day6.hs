{-# LANGUAGE LambdaCase #-}
module Day6
    ( 
        part1,
        part2
    ) where

import Control.Monad

import Data.List
import Data.List.Split (splitOn)

import Data.Either
import Data.Set hiding (take, drop, foldl, map, (\\), foldl', foldr)
import Text.Parsec
import Text.Parsec.String

parseInput :: String -> Either ParseError [String]
parseInput input =  mapM (parse parseChar "") $ splitOnAnyOf ["\n\n"] input

parseChar :: Parser String
parseChar = (many letter  `sepBy` endOfLine) >>= \x -> eof >> pure (toList . fromList $ concat x)

splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

solve1 :: [String] -> Int
solve1 = sum . map length

part1 ::  String -> String
part1 input = show output
    where 
        output = case parseInput input of
            Right r -> solve1 r
            Left e -> 123

solve2 :: [String] -> [Int]
solve2 input = [(head list)..(last list)] \\ list
    where list = sort . map solve1 $ [input]

part2 ::  String -> String
part2 input = input