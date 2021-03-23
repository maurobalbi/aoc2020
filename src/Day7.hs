{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day7
    ( 
        part1,
        part2
    ) where

import Control.Monad

import Data.List
import Data.List.Split (splitOn)

import Data.Either
import Data.Maybe
import Data.String.QQ
import qualified Data.Graph as G
import qualified Data.Set as S
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String

data Bag = Bag {
    name:: String,
    contains:: [(Int, String)]
} deriving (Show, Eq, Ord)

parseInput :: String -> Either ParseError [Bag]
parseInput input =  mapM (parse parseBag "") $ splitOnAnyOf ["\n"] input

parseBag :: Parser Bag
parseBag = Bag <$> parseName <*> parseContainedBags

parseName :: Parser [Char]
parseName = (++) <$> (many letter <* space) <*> many letter <* space

parseContainedBags :: Parser [(Int, [Char])]
parseContainedBags = string "bags contain " *> parseBagCount `sepBy` string ", "
    where
        parseBagCount = do
            count <- many1 digit 
            space
            bag <- (++) <$> (many letter <* space) <*> many letter
            space
            try (string "bags") <|> string "bag"
            return (read count, bag)

splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

findGoldBags :: [Bag] -> [Bag]
findGoldBags = filter (\x -> "shinygold" `elem` containedBags x)
    where 
        containedBags = map snd . contains

findParentBags :: [Bag] -> Bag -> [Bag]
findParentBags bags bag = 
    let bagToNode = \x -> (x, name x, map snd $ contains x)
        (graph, nodeFromVertex, vertexFromKey) = G.graphFromEdges . map bagToNode $ bags
        ancestors = G.reachable (G.transposeG graph) <$> vertexFromKey (name bag)
        getNodePart (n, _, _) = n
    in map (getNodePart . nodeFromVertex) $ fromMaybe mempty ancestors

solve1:: [Bag] -> Int
solve1 x = length . S.fromList $ concatMap (findParentBags x) (findGoldBags x)

part1 ::  String -> String
part1 input = show output
    where 
        output = case parseInput input of
            Right r -> show . solve1 $ r
            Left e -> show e

findGoldBag :: [Bag] -> Bag
findGoldBag = head . filter (\x -> "shinygold" == name x)

findChildBags :: [Bag] -> Bag -> [Bag]
findChildBags bags bag = 
    let bagToNode = \x -> (x, name x, map snd $ contains x)
        (graph, nodeFromVertex, vertexFromKey) = G.graphFromEdges . map bagToNode $ bags
        ancestors = G.reachable graph <$> vertexFromKey (name bag)
        getNodePart (n, _, _) = n
    in map (getNodePart . nodeFromVertex) $ fromMaybe mempty ancestors

solve2 :: [Bag] -> [Bag]
solve2 x = findChildBags x (findGoldBag x)

part2 ::  String -> String
part2 input = show output
    where 
        output = case parseInput input of
            Right r -> show $ solve2 r
            Left e -> show [1]

testInput :: String
testInput = 
    [s|
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.|]
