module Main where

import Language
import PrettyPrint
import Parser



program0 = "3 + 2 >= 5"
program1 = "2 - 1 >="
program2 = "2 - 1 >"

main :: IO ()
main = do
  putStrLn $ show $ clex program0
  putStrLn $ show $ clex program1
  putStrLn $ show $ clex program2
