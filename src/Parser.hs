module Parser where

import Data.Char
import Language



type Token = String
type LineNum = Int

parse :: String -> CoreProgram
parse = syntax . (clex 0)

clex :: LineNum -> String -> [(LineNum, Token)]
clex _ [] = []
clex lineNum (c : cs)
  | c == '\n' = clex (lineNum + 1) cs
clex lineNum (c : cs)
  | isWhiteSpace c = clex lineNum cs
clex lineNum (c : cs)
  | isDigit c = (lineNum, num_token) : clex lineNum rest_cs
                where
                  num_token = c : takeWhile isDigit cs
                  rest_cs = dropWhile isDigit cs
clex lineNum (c : cs)
  | isAlpha c = (lineNum, var_tok) : clex lineNum rest_cs
                where
                  var_tok = c : takeWhile isIdChar cs
                  rest_cs = dropWhile isIdChar cs
clex lineNum (c : d : cs)
  | [c, d] == "||" = clex (lineNum + 1) $ chompUntilNewline cs
clex lineNum (c : d : cs)
  | [c, d] `elem` twoCharOps = (lineNum, [c, d]) : clex lineNum cs
clex lineNum (c : cs)
  = (lineNum, [c]) : clex lineNum cs

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t"

chompUntilNewline :: String -> String
chompUntilNewline [] = []
chompUntilNewline (c : cs)
  | c == '\n' = cs
  | otherwise = chompUntilNewline cs

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

syntax :: [(LineNum, Token)] -> CoreProgram
syntax = undefined
