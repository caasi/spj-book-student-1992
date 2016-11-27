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

main :: IO ()
main = do
  let showGreetingsN = (show . pGreetingsN . clex 0)
  putStrLn $ showGreetingsN program2
  putStrLn $ showGreetingsN program3
