module Day2
    ( 
        part1,
        part2
    ) where

import Control.Monad
import Data.Char
import Data.List
import Text.Parsec
import Text.Parsec.String

data Input = Input {
    min:: Int,
    max:: Int,
    test:: Char,
    term:: String
} deriving (Show)

parseInput :: String -> Either ParseError [Input]
parseInput = parse parseFile "" 

parseFile :: Parser [Input]
parseFile = manyTill parseToInput eof

parseToInput:: Parser Input
parseToInput = do 
    min <- parseInt
    char '-'
    max <- parseInt
    many1 space
    test <- anyChar
    char ':'
    many1 space
    term <- manyTill anyChar (eof <|> void newline)
    pure (Input min max test term)

parseInt :: Parser Int
parseInt = read <$> many digit

isValid1 :: Input -> Bool
isValid1 (Input min max test term) = 
    occurence >= min && occurence <= max
        where 
            occurence = letterFrequency test term

letterFrequency:: Char -> String -> Int
letterFrequency c s = snd . letterPair . safeHead . filter (\x -> fst x == c) $ frequency s
    where 
        letterPair (Just a)= a
        letterPair Nothing = (c, 0)

safeHead:: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

frequency:: (Eq a, Ord a) => [a] -> [(a, Int)]
frequency =  map (\x -> (head x, length x)) . group . sort

part1 ::  String -> String
part1 input = output
    where 
        output = case parseInput input of
            Right r -> show . length . filter (True ==) . map isValid1 $ r
            Left e -> show e

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

isValid2 :: Input -> Bool
isValid2 (Input min max test term) = isElementAtIndex (min - 1) test term `xor` isElementAtIndex (max - 1) test term

isElementAtIndex :: Int -> Char -> String -> Bool 
isElementAtIndex i c s = case safeHead . take 1 . drop i $ s of
    Just char -> char == c
    Nothing -> False

part2 ::  String -> String
part2 input = output
    where 
        output = case parseInput input of
            Right r ->  show . length . filter (True ==) . map isValid2 $ r
            Left e -> show e