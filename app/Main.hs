module Main where

import Language
import PrettyPrint



main :: IO ()
main = do
  putStrLn $ pprExpr $ mkMultiAp 42 (EVar "f") (EVar "g")
