module Main where

import Language
import PrettyPrint
import Parser
import TiState
import Utils



program = "\
  \main = S K K 3\n\
\"

main :: IO ()
main = do
  --let coreProgram = parse program
  --let states@(stack, dump, (_, _, heap), globals, stats) = compile coreProgram
  --(putStrLn . show) $ (stack, heap, globals, stats)
  (putStrLn . runProg) program
