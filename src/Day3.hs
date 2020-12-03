module Day3
    ( 
        part1,
        part2
    ) where

import Day2(safeHead)
import Control.Monad
import Data.Char
import Data.List
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

part1 ::  String -> String
part1 input = output
    where 
        output = case parseInput input of
            Right r -> solve1 r
            Left e -> show e

solve1 :: [[Terrain]] -> String
solve1 input = show . length $ findTrees 3 input

findTrees :: Int -> [[Terrain]] -> [Terrain]
findTrees slope input = filter (== Tree) $ passTerrain slope input

passTerrain :: Int-> [[a]] -> [a]
passTerrain slope = pass slope . (map cycle)

pass:: Int -> [[a]] -> [a]
pass slope input = foldr (\x y-> (head (drop (slope * length y) x)) : y ) [] $ reverse input

nthTerrain :: Int -> [Terrain] -> Terrain
nthTerrain n input = head . drop n $ cycle input

part2 ::  String -> String
part2 input = output
    where 
        output = case parseInput input of
            Right r -> solve1 r
            Left e -> show e

solve2 :: (Eq b, Num b) => [b] -> b -> [b]
solve2 i test = 
  do x <- i
     y <- i
     z <- i
     guard ((x + y + z) == test)
     pure (x * y * z)