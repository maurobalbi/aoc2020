{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import Data.List
import Data.List.Split (splitOn)
import Control.Monad
import Text.Parsec
import Data.String.QQ
import Text.Parsec.String
import Text.Parsec.Perm

data IsValid = Yes | No deriving (Show, Eq)

data PassPort = PassPort {
    birthYear:: String,
    issueYear:: String,
    height::String,
    expirationYear:: String,
    hairColor:: String,
    eyeColor:: String,
    passportId:: String,
    countryId:: String
} deriving (Show)

splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

parseProp :: String -> Parser String
parseProp s = do 
    string $ s ++ ":"
    manyTill anyChar (space <|> (eof >> pure ' '))

parsePassPort :: Parser PassPort
parsePassPort = permute (PassPort <$$> try (parseProp "byr")
                        <||> try (parseProp "iyr")
                        <||> try (parseProp "hgt")
                        <||> try (parseProp "eyr")
                        <||> try (parseProp "hcl")
                        <||> try (parseProp "ecl")
                        <||> try (parseProp "pid")
                        <|?> ("_", try (parseProp "cid")))

parseChunk :: Parser IsValid
parseChunk = try parseValid <|> parseInvalid
    where 
        parseValid = do
            parsePassPort
            eof
            pure Yes
        parseInvalid = do
            manyTill anyChar eof
            pure No

parseInput :: String -> Either ParseError [IsValid]
parseInput input = mapM (parse parseChunk "") splitInput
    where
        splitInput = splitOnAnyOf ["\n\n"] input

solve1 :: [IsValid] -> String
solve1  = show . length . filter (== Yes)

part1 ::  String -> String
part1 input = output
    where 
        output = case parseInput input of
            Right r -> solve1 r
            Left e -> show e

testInput :: String
testInput = [s|
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in

ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
|]

solve2 :: p -> [Char]
solve2 input = ""

part2 ::  String -> String
part2 input = output
    where 
        output = case parseInput input of
            Right r -> solve2 r
            Left e -> show e

            