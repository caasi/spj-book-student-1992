module Main where

import Language
import PrettyPrint
import Parser



program = "\
  \f = 3 ;\n\
  \g x y = let z = x in z ;\n\
  \h x = case (let y = x in y) of\n\
  \        <1> -> 2 ;\n\
  \        <2> -> 5\n\
\"

main :: IO ()
main = do
  (putStrLn . show . parse) program
