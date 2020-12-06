{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day4 where

import Control.Monad
import Control.Exception hiding (try)
import Data.List
import Data.List.Split (splitOn)
import Data.String.QQ
import Text.Parsec
import Text.Parsec.Perm
import Text.Parsec.String

data IsValid = Yes | No deriving (Show, Eq)

data PassPort = PassPort
  { birthYear :: String,
    issueYear :: String,
    height :: String,
    expirationYear :: String,
    hairColor :: String,
    eyeColor :: String,
    passportId :: String,
    countryId :: String
  }
  deriving (Show)

splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

parseProp :: String -> Parser String
parseProp s = do
  string $ s ++ ":"
  manyTill anyChar (space <|> (eof >> pure ' '))

parsePassPort :: Parser PassPort
parsePassPort =
  permute
    ( PassPort <$$> try (parseProp "byr")
        <||> try (parseProp "iyr")
        <||> try (parseProp "hgt")
        <||> try (parseProp "eyr")
        <||> try (parseProp "hcl")
        <||> try (parseProp "ecl")
        <||> try (parseProp "pid")
        <|?> ("_", try (parseProp "cid"))
    )

parseChunk :: Parser PassPort -> Parser IsValid
parseChunk pPP = try parseValid <|> parseInvalid
  where
    parseValid = do
      pPP
      eof
      pure Yes
    parseInvalid = do
      manyTill anyChar eof
      pure No

parseInput :: String -> Either ParseError [IsValid]
parseInput input = mapM (parse (parseChunk parsePassPort) "") splitInput
  where
    splitInput = splitOnAnyOf ["\n\n"] input

solve1 :: [IsValid] -> String
solve1 = show . length . filter (== Yes)

part1 :: String -> String
part1 input = output
  where
    output = case parseInput input of
      Right r -> solve1 r
      Left e -> show e

testInput :: String
testInput =
  [s|
eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007
|]

parseProp2 :: Parser a -> String -> Parser a
parseProp2 p s = do
  string $ s ++ ":"
  str <- p
  space <|> (eof >> pure ' ')
  pure str

parseByr :: Parser String
parseByr = do
    n <- count 4 digit
    if read n >= 1920 && read n <= 2002 then return n
    else mzero

parseIyr :: Parser String
parseIyr = do
    n <- count 4 digit
    if read n >= 2010 && read n <= 2020 then return n
    else mzero

parseEyr :: Parser String
parseEyr = do
    n <- count 4 digit
    if read n >= 2020 && read n <= 2030 then return n
    else mzero

parseEcl :: Parser String
parseEcl = try (string "amb") 
    <|> try (string "blu") 
    <|> try (string "brn") 
    <|> try (string "gry") 
    <|> try (string "grn") 
    <|> try (string "hzl") 
    <|> try (string "oth")   

parsePid :: Parser String
parsePid = count 9 digit

parseHgt :: Parser String
parseHgt = do
    n <- many digit
    s <- count 2 letter
    let num = read n
    case s of
        "cm" -> if num >= 150 && num <= 193 then return $ n ++ s else mzero
        "in" -> if num >= 59 && num <= 76 then return $ n ++ s else mzero

parseHcl :: Parser String
parseHcl = do
    hash <- char '#'
    digits <- count 6 $ digit <|> oneOf ['a'..'f']
    return $ hash : digits

parsePassPort2 :: Parser PassPort
parsePassPort2 =
  permute
    ( PassPort <$$> try (parseProp2 parseByr "byr")
        <||> try (parseProp2 parseIyr "iyr")
        <||> try (parseProp2 parseHgt "hgt")
        <||> try (parseProp2 parseEyr "eyr")
        <||> try (parseProp2 parseHcl "hcl")
        <||> try (parseProp2 parseEcl "ecl")
        <||> try (parseProp2 parsePid "pid")
        <|?> ("_", try (parseProp "cid"))
    )

parseInput2 :: String -> Either ParseError [IsValid]
parseInput2 input = mapM (parse (parseChunk parsePassPort2) "") splitInput
  where
    splitInput = splitOn "\n\n" input

solve2 :: [IsValid] -> String
solve2 = show . length . filter (== Yes)

part2 :: String -> String
part2 input = output
  where
    output = case parseInput2 input of
      Right r -> solve2 r
      Left e -> show e