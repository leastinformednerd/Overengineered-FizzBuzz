module Parser(parseProgram, parseProgramLines, AbstractSyntaxList) where

import Text.Read
import Data.Maybe

-- This is such a stupid name but I can't be bothered coming up with a better one
type AbstractSyntaxList = [(Int, String)]

parseLine :: String -> Maybe (Int, String)
parseLine = parseComponents . words  
  where
    parseComponents (base : message) = fmap (\int -> (int, concat message)) (readMaybe base)
    parseComponents _ = Nothing

parseProgram :: String -> AbstractSyntaxList
parseProgram = parseProgramLines . lines

parseProgramLines :: [String] -> AbstractSyntaxList
parseProgramLines input =  mapMaybe parseLine input
