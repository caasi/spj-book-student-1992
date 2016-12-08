module Main where

import Language
import PrettyPrint
import Parser
import MultState



program = "\
  \f = 3;\n\
  \g x y = let z = x in z;\n\
  \h x = case (let y = x in y) of\n\
  \        <1> -> 2 + 1 * x;\n\
  \        <2> -> 6\n\
\"

main :: IO ()
main = do
  (putStrLn . show . parse) program
  (putStr . layn) $ map show (evalMult (2, 3, 0, 0))
