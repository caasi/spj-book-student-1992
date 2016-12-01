module Parser where

import Data.Char
import Language



parse :: String -> CoreProgram
parse = syntax . (clex 0)



-- the lexer
type LineNum = Int
type Token = (LineNum, String)

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



-- the parser
syntax :: [Token] -> CoreProgram
syntax = take_first_parse . pProgram
         where
           take_first_parse ((prog, []) : others) = prog
           take_first_parse (parse      : others) = take_first_parse others
           take_first_parse others                = error $ "Syntax error: " ++ (show others)



type Parser a = [Token] -> [(a, [Token])]

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack{", "}", "=", "->", ",", ";"]

pSat :: (String -> Bool) -> Parser String
pSat f [] = []
pSat f ((lineNum, tok):toks)
  | f tok     = [(tok, toks)]
  | otherwise = []

pApply :: Parser a -> (a -> b) -> Parser b
pApply p1 f toks = flip map (p1 toks) (\(a, toks1) -> (f a, toks1))

pFmap :: (a -> b) -> Parser a -> Parser b
pFmap = flip pApply

pAp :: Parser (a -> b) -> Parser a -> Parser b
pAp pf p toks
  = [ (f v, toks2) | (f, toks1) <- pf toks
                   , (v, toks2) <- p toks1
                   ]

pLit :: String -> Parser String
pLit s = pSat (== s)

pVar :: Parser Name
pVar = pSat (flip notElem keywords)

pNum :: Parser Int
pNum = (pSat (and . map isNumber)) `pApply` read

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 = combine `pFmap` p1 `pAp` p2

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 = combine `pFmap` p1 `pAp` p2 `pAp` p3

pThen4 :: (a -> b -> c -> d -> e)
       -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 = combine `pFmap` p1 `pAp` p2 `pAp` p3 `pAp` p4

pThen5 :: (a -> b -> c -> d -> e -> f)
       -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
pThen5 combine p1 p2 p3 p4 p5 = combine `pFmap` p1 `pAp` p2 `pAp` p3 `pAp` p4 `pAp` p5

pEmpty :: a -> Parser a
pEmpty a = \toks -> [(a, toks)]

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (take 1) . ((pOneOrMore p) `pAlt` (pEmpty []))

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 p2 = pThen (:) p1 (pSepWithOneOrMore p2 p1)

pSepWithOneOrMore :: Parser b -> Parser a -> Parser [a]
pSepWithOneOrMore p2 p1
  = (take 1) . ((pThen (flip const) p2 (pOneOrMoreWithSep p1 p2)) `pAlt` (pEmpty []))

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr
      where
        mk_sc n ns _ expr = (n, ns, expr)

-- atomic expression
pAexpr :: Parser CoreExpr
pAexpr
    -- var
  = ( EVar `pFmap` pVar ) `pAlt`
    -- num
    ( ENum `pFmap` pNum ) `pAlt`
    -- Pack{num,num}
    ( pThen5
        (\_ tag _ as _ -> EConstr tag as)
        (pLit "Pack{")
        pNum
        (pLit ",")
        pNum
        (pLit "}")
    ) `pAlt`
    -- ( expr )
    ( pThen3
        (\_ expr _ -> expr)
        (pLit "(")
        pExpr
        (pLit ")")
    )

pExpr :: Parser CoreExpr
pExpr
    -- expr aexpr
  = ( (pOneOrMore pAexpr) `pApply` mk_ap_chain ) `pAlt`
    -- let defns in expr
    -- letrec defns in expr
    ( pThen4
        (\isRec namedExprs _ expr -> ELet isRec namedExprs expr)
        (((pLit "letrec") `pAlt` (pLit "let")) `pApply` isRecursive)
        ( pOneOrMoreWithSep
            ( pThen3
                (\name _ expr -> (name, expr))
                pVar
                (pLit "=")
                pExpr
            )
            (pLit ";")
        )
        (pLit "in")
        pExpr
    ) `pAlt`
    -- case expr of alts
    ( pThen4
        (\_ expr _ alters -> ECase expr alters)
        (pLit "case")
        pExpr
        (pLit "of")
        (pOneOrMoreWithSep pAlter (pLit ";"))
    ) `pAlt`
    -- \ var1...varN . expr
    ( pThen4
        (\_ vars _ expr -> ELam vars expr)
        (pLit "\\")
        (pOneOrMore pVar)
        (pLit ".")
        pExpr
    ) `pAlt`
    -- paexpr
    ( pAexpr )

mk_ap_chain :: [CoreExpr] -> CoreExpr
mk_ap_chain [] = error "Syntax error: EAp"
mk_ap_chain (x:[]) = x
mk_ap_chain (x:y:xs) = mk_ap_chain (EAp x y : xs)

pAlter :: Parser CoreAlt
pAlter = pThen4 mk_alter pTag (pZeroOrMore pVar) (pLit "->") pExpr
         where
           mk_alter id vars _ expr = (id, vars, expr)

pTag :: Parser Int
pTag = pThen3 (\_ tag _ -> tag) (pLit "<") pNum (pLit ">")
