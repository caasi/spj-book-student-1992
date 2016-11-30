module Parser where

import Data.Char
import Language



type LineNum = Int
type Token = (LineNum, String)

parse :: String -> CoreProgram
parse = syntax . (clex 0)

clex :: LineNum -> String -> [Token]
clex _ [] = []
clex lineNum (c : cs)
  | c == '\n' = clex (lineNum + 1) cs
clex lineNum (c : cs)
  | isWhiteSpace c = clex lineNum cs
clex lineNum (c : cs)
  | isDigit c = (lineNum, num_tok) : clex lineNum rest_cs
                where
                  num_tok = c : takeWhile isDigit cs
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



syntax :: [Token] -> CoreProgram
syntax = undefined

type Parser a = [Token] -> [(a, [Token])]

pSat :: (String -> Bool) -> Parser String
pSat f [] = []
pSat f ((lineNum, tok):toks)
  | f tok     = [(tok, toks)]
  | otherwise = []

pLit :: String -> Parser String
pLit s = pSat (== s)

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

pVar :: Parser String
pVar = pSat (flip notElem keywords)

pNum :: Parser Int
pNum = (pSat (and . map isNumber)) `pApply` read

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks
  = [ (combine v1 v2, toks2) | (v1, toks1) <- p1 toks
                             , (v2, toks2) <- p2 toks1
    ]

pGreeting :: Parser (String, String)
pGreeting
  = pThen keep_first
          (pThen ((,)) pHelloOrGoodbye pVar)
          (pLit "!")
    where
      keep_first a b = a

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (take 1) . ((pOneOrMore p) `pAlt` (pEmpty []))

-- # looks like `pure`
pEmpty :: a -> Parser a
pEmpty a = \toks -> [(a, toks)]

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

pGreetingsN :: Parser Int
pGreetingsN = (pZeroOrMore pGreeting) `pApply` length

pApply :: Parser a -> (a -> b) -> Parser b
pApply p1 f toks = flip map (p1 toks) (\(a, toks1) -> (f a, toks1))

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 p2 = pThen (:) p1 (pSepWithOneOrMore p2 p1)

pSepWithOneOrMore :: Parser b -> Parser a -> Parser [a]
pSepWithOneOrMore p2 p1
  = (take 1) . ((pThen (flip const) p2 (pOneOrMoreWithSep p1 p2)) `pAlt` (pEmpty []))

pOneOrMoreGreetingsWithSep :: Parser [(String, String)]
pOneOrMoreGreetingsWithSep
  = pOneOrMoreWithSep (pThen ((,)) pHelloOrGoodbye pVar) (pLit ";")

pNumbers :: Parser [Int]
pNumbers = pOneOrMore pNum
