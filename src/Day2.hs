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
    space
    test <- anyChar
    char ':'
    space
    term <- manyTill anyChar (eof <|> void newline)
    pure (Input min max test term)

parseInt :: Parser Int
parseInt = read <$> many digit

part1 ::  String -> String
part1 input = output
    where 
        output = case parseInput input of
            Right r -> show . length . filter ((==) True) . map isValid $ r
            Left e -> show e

isValid :: Input -> Bool
isValid (Input min max test term) = 
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

part2 ::  String -> String
part2 input = "Not implemented"