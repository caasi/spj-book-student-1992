module Main where

import Language

main :: IO ()
main = do
  let
    coreProgram :: CoreProgram
    coreProgram =
      [ ("main", [], (EAp (EVar "double") (ENum 21)))
      , ("double", ["x"], (EAp (EAp (EVar "+") (EVar "x")) (EVar "x")))
      ]
  putStrLn $ show coreProgram
