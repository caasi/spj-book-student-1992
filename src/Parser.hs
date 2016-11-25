module Parser where

import Data.Char
import Language



type Token = String

parse :: String -> CoreProgram
parse = syntax . clex

clex :: String -> [Token]
clex [] = []
clex (c : cs)
  | isWhiteSpace c = clex cs
clex (c : cs)
  | isDigit c = num_token : clex rest_cs
                where
                  num_token = c : takeWhile isDigit cs
                  rest_cs = dropWhile isDigit cs
clex (c : cs)
  | isAlpha c = var_tok : clex rest_cs
                where
                  var_tok = c : takeWhile isIdChar cs
                  rest_cs = dropWhile isIdChar cs
clex (c : cs)
  | c == '|' = clex $ chompComment cs
clex (c : d : cs)
  | [c, d] `elem` twoCharOps = [c, d] : clex cs
clex (c : cs)
  = [c] : clex cs

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"

chompComment :: String -> String
chompComment [] = []
chompComment (c : cs)
  | c == '|'  = chompUntilNewline cs
  | otherwise = '|' : cs

chompUntilNewline :: String -> String
chompUntilNewline [] = []
chompUntilNewline (c : cs)
  | c == '\n' = cs
  | otherwise = chompUntilNewline cs

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

syntax :: [Token] -> CoreProgram
syntax = undefined
