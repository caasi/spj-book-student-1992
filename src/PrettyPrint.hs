module PrettyPrint where

import Language
import Utils



pprint :: CoreProgram -> String
pprint = iDisplay . pprProgram

pprProgram :: CoreProgram -> Iseq
pprProgram prog = iInterleave sep (map pprCoreScDefn prog)
  where sep = iConcat [ iStr " ;", iNewline ]

pprCoreScDefn :: CoreScDefn -> Iseq
pprCoreScDefn (name, names, expr) =
  iConcat [ iStr $ unwords (name : names), iStr " = ", pprExpr 0 expr ]

toPrecedence :: [Char] -> Int
toPrecedence "|"  = 1
toPrecedence "&"  = 2
toPrecedence "==" = 3
toPrecedence "~=" = 3
toPrecedence ">"  = 3
toPrecedence ">=" = 3
toPrecedence "<"  = 3
toPrecedence "<=" = 3
toPrecedence "-"  = 4
toPrecedence "+"  = 4
toPrecedence "/"  = 5
toPrecedence "*"  = 5
toPrecedence _    = 6

pprExpr :: Int -> CoreExpr -> Iseq
pprExpr outter (ENum n) = iStr $ show n
pprExpr outter (EVar v) = iStr v
pprExpr outter (EAp (EAp (EVar op) e1) e2)
  | outter > inner  = iConcat [ iStr "(", seq, iStr ")" ]
  | outter <= inner = seq
  where
    inner = toPrecedence op
    seq = iConcat [ pprExpr inner e1, iStr " ", iStr op, iStr " ", pprExpr inner e2
                  ]
pprExpr outter (EAp e1 e2) = (pprExpr 6 e1) `iAppend` (iStr " ") `iAppend` (pprExpr 6 e2)
pprExpr outter (ELet isrec defns expr) =
  iConcat [ iStr keyword, iNewline
          , iStr "  ", iIndent (pprDefns defns), iNewline
          , iStr "in ", pprExpr 0 expr
          ]
  where
    keyword
      | not isrec = "let"
      | isrec = "letrec"
pprExpr outter (ECase e alts) =
  iConcat [ iStr "case ", pprExpr 0 e, iStr " of", iNewline
          , iStr "  ", iIndent (pprAlts alts), iNewline
          ]
pprExpr outter (ELam vars e) =
  iConcat [ iStr "\\", iStr $ unwords vars, iStr " -> ", pprExpr 0 e ]

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where sep = iConcat [ iStr ";", iNewline ]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) = iConcat [ iStr name, iStr " = ", iIndent (pprExpr 0 expr) ]

pprAlts :: [Alter Name] -> Iseq
pprAlts alts = iInterleave iNewline (map pprAlt alts)

pprAlt :: Alter Name -> Iseq
pprAlt (tag, vars, expr) =
  iConcat [ iStr "Pack{ ", iStr tagName, iStr ", ", iStr numVars, iStr "} "
          , iStr $ unwords vars, iStr " = ", pprExpr 0 expr
          ]
  where
    tagName = show tag
    numVars = show $ length vars



--mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
--mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s) where e2s = e2 : e2s



data Iseq
  = INil
  | IStr String
  | IAppend Iseq Iseq
  | INewline
  | IIndent Iseq

iNil     :: Iseq                  -- The empty iseq
iStr     :: String -> Iseq        -- Turn a string into an iseq
iAppend  :: Iseq -> Iseq -> Iseq  -- Append two iseq
iNewline :: Iseq                  -- New line with indentation
iIndent  :: Iseq -> Iseq          -- Indent an iseq
iDisplay :: Iseq -> String        -- Turn an iseq into a string

iNil = INil
iStr = IStr
iAppend INil seq = seq
iAppend seq INil = seq
iAppend seq1 seq2 = IAppend seq1 seq2
iNewline = INewline
iIndent seq = IIndent seq
iDisplay seq = flatten 0 [(seq, 0)]

iConcat :: [Iseq] -> Iseq
iConcat = foldl iAppend iNil

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _ [] = iNil
iInterleave _ (s : []) = s
iInterleave seq (s : ss) = s `iAppend` seq `iAppend` (iInterleave seq ss)

flatten :: Int -> [(Iseq, Int)] -> String
flatten col []
  = ""
flatten col ((INil, indent) : seqs)
  = flatten col seqs
flatten col ((IStr s, indent) : seqs)
  = s ++ (flatten (col + length s) seqs)
flatten col ((IAppend seq1 seq2, indent) : seqs)
  = flatten col ((seq1, indent) : (seq2, indent) : seqs)
flatten col ((INewline, indent) : seqs)
  = '\n' : (spaces indent) ++ flatten indent seqs
flatten col ((IIndent seq, indent) : seqs)
  = flatten col ((seq, col) : seqs)

spaces = flip replicate ' '

-- other useful functions
iNum :: Int -> Iseq
iNum n = iStr $ show n

iFWNum :: Int -> Int -> Iseq
iFWNum width n
  = iStr (spaces (width - length digits) ++ digits)
    where
      digits = show n

iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (map lay_item (zip [1..] seqs))
             where
               lay_item (n, seq)
                 = iConcat [ iFWNum 4 n, iStr ") ", iIndent seq, iNewline
                           ]
