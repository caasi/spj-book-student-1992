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



data PartialExpr = NoOp | FoundOp Name CoreExpr

type Parser a = [Token] -> [(a, [Token])]

keywords :: [String]
keywords
  = [ "let", "letrec"
    , "case", "in", "of"
    , "Pack", "{", ",", "}"
    , "=", "->"
    , ";"
    , "&", "|", "+", "-", "*", "/"
    ] ++ relOps

relOps :: [String]
relOps = ["<", "<=", "==", "~=", ">=", ">"]

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
pVar = pSat (\s@(c:ss) -> s `notElem` keywords && isAlpha c && (and . map isAlphaNum) ss)

pNum :: Parser Int
pNum = read `pFmap` (pSat (and . map isDigit))

pRelOp :: Parser Name
pRelOp = pSat (flip elem relOps)

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

pThen6 :: (a -> b -> c -> d -> e -> f -> g)
       -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g
pThen6 combine p1 p2 p3 p4 p5 p6 = combine `pFmap` p1 `pAp` p2 `pAp` p3 `pAp` p4 `pAp` p5 `pAp` p6

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
    ( pThen6
        (\_ _ tag _ as _ -> EConstr tag as)
        (pLit "Pack")
        (pLit "{")
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
    -- letrec, let defns in expr
  = ( pThen4
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
    -- expr1
    pExpr1

pAlter :: Parser CoreAlt
pAlter = pThen4 mk_alter pTag (pZeroOrMore pVar) (pLit "->") pExpr
         where
           mk_alter id vars _ expr = (id, vars, expr)

pTag :: Parser Int
pTag = pThen3 (\_ tag _ -> tag) (pLit "<") pNum (pLit ">")

pExpr1 :: Parser CoreExpr
pExpr1 = assembleOp `pFmap` pExpr2 `pAp` pExpr1c

pExpr1c :: Parser PartialExpr
pExpr1c = (FoundOp `pFmap` (pLit "|") `pAp` pExpr1) `pAlt` (pEmpty NoOp)

pExpr2 :: Parser CoreExpr
pExpr2 = assembleOp `pFmap` pExpr3 `pAp` pExpr2c

pExpr2c :: Parser PartialExpr
pExpr2c = (FoundOp `pFmap` (pLit "&") `pAp` pExpr2) `pAlt` (pEmpty NoOp)

pExpr3 :: Parser CoreExpr
pExpr3 = assembleOp `pFmap` pExpr4 `pAp` pExpr3c

pExpr3c :: Parser PartialExpr
pExpr3c = (FoundOp `pFmap` pRelOp `pAp` pExpr4) `pAlt` (pEmpty NoOp)

pExpr4 :: Parser CoreExpr
pExpr4 = assembleOp `pFmap` pExpr5 `pAp` pExpr4c

pExpr4c :: Parser PartialExpr
pExpr4c = (FoundOp `pFmap` (pLit "+") `pAp` pExpr4) `pAlt`
          (FoundOp `pFmap` (pLit "-") `pAp` pExpr5) `pAlt`
          (pEmpty NoOp)

pExpr5 :: Parser CoreExpr
pExpr5 = assembleOp `pFmap` pExpr6 `pAp` pExpr5c

pExpr5c :: Parser PartialExpr
pExpr5c = (FoundOp `pFmap` (pLit "*") `pAp` pExpr5) `pAlt`
          (FoundOp `pFmap` (pLit "/") `pAp` pExpr6) `pAlt`
          (pEmpty NoOp)

pExpr6 :: Parser CoreExpr
pExpr6 = (pOneOrMore pAexpr) `pApply` mk_ap_chain

mk_ap_chain :: [CoreExpr] -> CoreExpr
mk_ap_chain [] = error "Syntax error: EAp"
mk_ap_chain (x:[]) = x
mk_ap_chain (x:y:xs) = mk_ap_chain (EAp x y : xs)

assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2
