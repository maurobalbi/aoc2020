{-# LANGUAGE QuasiQuotes #-}

module Day3
    ( 
        part1,
        part2
    ) where


import Day2(safeHead)
import Control.Monad
import Data.Char
import Data.List

import Data.String.QQ
import Text.Parsec
import Text.Parsec.String

data Terrain = Tree | Snow deriving ( Show, Eq)

parseInput :: String -> Either ParseError [[Terrain]]
parseInput = parse parseAll ""

parseAll :: Parser [[Terrain]]
parseAll = manyTill parseLine eof

parseLine :: Parser [Terrain]
parseLine = manyTill parseTerrain $ eof <|> void newline

parseTerrain :: Parser Terrain
parseTerrain = char '#' *> pure Tree <|> char '.' *> pure Snow


findTrees :: (Int, Int) -> [[Terrain]] -> [Terrain]
findTrees slope input = filter (== Tree) $ passTerrain slope input

passTerrain :: (Int, Int)-> [[a]] -> [a]
passTerrain slope = pass slope . map cycle

pass:: (Int, Int) -> [[a]] -> [a]
pass slope input = foldr (\x y-> head (drop (fst slope * length y) x) : y ) [] $ reverse $ everyf (snd slope) input

solve1 :: [[Terrain]] -> String
solve1 input = show . length $ findTrees (3, 1) input

part1 ::  String -> String
part1 input = output
    where 
        output = case parseInput input of
            Right r -> solve1 r
            Left e -> show e

everyf :: Int -> [a] -> [a]
everyf n xs = case xs of
              (y:ys) -> y : everyf n (drop (n-1) ys)
              [] -> []

slopes :: [(Int, Int)]
slopes = [(1,1), (3,1), (5,1), (7,1), (1, 2)]

slopeFunctions :: [[[Terrain]] -> Int]
slopeFunctions = map treeCount slopes 
    where
        treeCount = \x -> length . findTrees x

solve2 input = show . product $ slopeFunctions <*> pure input

part2 ::  String -> String
part2 input = output
    where 
        output = case parseInput input of
            Right r -> solve2 r
            Left e -> show e

testInput :: String
testInput = [s|
..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
|]