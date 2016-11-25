module Main where

import Language
import PrettyPrint
import Parser



program = "\
  \compose = f g -> f . g\n\
  \let a\n\
  \  = compose f g\n\
  \in a\
\"

program1 = "2 - 1 >="
program2 = "2 - 1 >"

main :: IO ()
main = do
  putStrLn $ show $ clex 0 program
