{-# LANGUAGE LambdaCase #-}
module Day5
    ( 
        part1,
        part2
    ) where

import Control.Monad

import Data.List
import Data.Set hiding (take, drop, foldl, map, (\\))
import Text.Parsec
import Text.Parsec.String

parseInput :: String -> Either ParseError [String]
parseInput = Right . lines

firstHalve :: [a] -> [a]
firstHalve list = take half list 
    where half = length list `div` 2

secondHalve :: [a] -> [a]
secondHalve list = drop half list 
    where half = length list `div` 2

calculateId :: ([Integer], [Integer]) -> Integer
calculateId = (\(row, column) -> head row * 8 + head column)

findSeat =  foldl takeHalf ([0..127], [0..7])
     where takeHalf y x = case x of
            'F' -> (firstHalve $ fst y, snd y)
            'B' -> (secondHalve $ fst y, snd y)
            'R' -> (fst y, secondHalve $ snd y)
            'L' -> (fst y, firstHalve $ snd y)
            _ -> ([],[])

solve1 :: String -> Int
solve1 = (\(row, column) -> head row * 8 + head column) . findSeat

part1 ::  String -> String
part1 input = output
    where 
        output = case parseInput input of
            Right r -> show . maximum . map solve1 $ r
            Left e -> show e


solve2 :: [String] -> [Int]
solve2 input = [(head list)..(last list)] \\ list
    where list = sort . map solve1 $ input

part2 ::  String -> String
part2 input = output
    where 
        output = case parseInput input of
            Right r ->  show . solve2 $ r
            Left e -> show e