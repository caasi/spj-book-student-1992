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
  iConcat [ iStr $ unwords (name : names), iStr " = ", pprExpr expr ]

pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n) = iStr $ show n
pprExpr (EVar v) = iStr v
pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)
pprExpr (ELet isrec defns expr) =
  iConcat [ iStr keyword, iNewline
          , iStr "  ", iIndent (pprDefns defns), iNewline
          , iStr "in ", pprExpr expr
          ]
  where
    keyword
      | not isrec = "let"
      | isrec = "letrec"
pprExpr (ECase e alts) =
  iConcat [ iStr "case ", pprExpr e, iStr " of", iNewline
          , iStr "  ", iIndent (pprAlts alts), iNewline
          ]
pprExpr (ELam vars e) =
  iConcat [ iStr "\\", iStr $ unwords vars, iStr " -> ", pprExpr e ]

pprAExpr :: CoreExpr -> Iseq
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise      = iConcat [ iStr "(", pprExpr e, iStr ")" ]

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where sep = iConcat [ iStr ";", iNewline ]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]

pprAlts :: [Alter Name] -> Iseq
pprAlts alts = iInterleave iNewline (map pprAlt alts)

pprAlt :: Alter Name -> Iseq
pprAlt (tag, vars, expr) =
  iConcat [ iStr "Pack{ ", iStr tagName, iStr ", ", iStr numVars, iStr "} "
          , iStr $ unwords vars, iStr " = ", pprExpr expr
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

iNil     :: Iseq                  -- The empty iseq
iStr     :: String -> Iseq        -- Turn a string into an iseq
iAppend  :: Iseq -> Iseq -> Iseq  -- Append two iseq
iNewline :: Iseq                  -- New line with indentation
iIndent  :: Iseq -> Iseq          -- Indent an iseq
iDisplay :: Iseq -> String        -- Turn an iseq into a string

iNil = INil
iStr = IStr
iAppend = IAppend
iNewline = iStr "\n"
iIndent seq = seq
iDisplay seq = flatten [seq]

iConcat :: [Iseq] -> Iseq
iConcat = foldl iAppend iNil

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _ [] = iNil
iInterleave _ (s : []) = s
iInterleave seq (s : ss) = s `iAppend` seq `iAppend` (iInterleave seq ss)

flatten :: [Iseq] -> String
flatten [] = ""
flatten (INil : seqs) = flatten seqs
flatten (IStr s : seqs) = s ++ (flatten seqs)
flatten (IAppend seq1 seq2 : seqs) = flatten (seq1 : seq2 : seqs)
