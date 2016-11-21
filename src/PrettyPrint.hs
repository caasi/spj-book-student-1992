module PrettyPrint where

import Language
import Utils



pprint :: CoreProgram -> String
pprint program = undefined

pprExpr :: CoreExpr -> String
pprExpr (ENum n) = show n
pprExpr (EVar v) = v
pprExpr (EAp e1 e2) = pprExpr e1 ++ " " ++ pprExpr e2
pprExpr (ELet isRec defns e) = "let " ++ go defns ++ "in " ++ pprExpr e where
  go [] = ""
  go ((name, e):ds) = name ++ " = " ++ pprExpr e ++ "; "
pprExpr (ECase e alts) = "case " ++ pprExpr e ++ " of " ++ go alts where
  go [] = ""
  go ((tag, vars, e):exprs) =
    "Pack{ " ++ show tag ++ ", " ++ show (length vars) ++ "} " ++ unwords vars ++ " = " ++ pprExpr e ++ "; "
pprExpr (ELam vars e) = "\\" ++ unwords vars ++ " -> " ++ pprExpr e

pprAExpr :: CoreExpr -> String
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise      = "(" ++ pprExpr e ++ ")"

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s) where e2s = e2 : e2s
