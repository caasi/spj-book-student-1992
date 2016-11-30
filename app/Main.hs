module Main where

import Language
import PrettyPrint
import Parser



program1 = "\
  \compose = f g -> f . g\n\
  \let a\n\
  \  = compose f g\n\
  \in a\
\"

program2 = "hello John ! goodbye James !"

program3 = "hello John !"

program4 = "hello John; goodbye James"

program5 = "2 3 5 7 11 13 17 19"

showProgram :: (Show a) => Parser a -> String -> String
showProgram p = show . p . clex 0

main :: IO ()
main = do
  putStrLn $ showProgram pGreetingsN program2
  putStrLn $ showProgram pGreetingsN program3
  putStrLn $ showProgram pOneOrMoreGreetingsWithSep program4
  putStrLn $ showProgram pNumbers program5
