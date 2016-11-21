module Main where

import Language
import PrettyPrint



main :: IO ()
main = do
  putStrLn $ pprint preludeDefns
