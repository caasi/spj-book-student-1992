module Main where

import Language
import PrettyPrint
import Parser
import TiState
import Utils



program = "\
  \main = S K K\n\
\"

main :: IO ()
main = do
  --let coreProgram = parse program
  --let states@(stack, dump, (_, _, heap), globals, stats) = step $ step $ step $ step $ compile coreProgram
  --(putStrLn . show) $ (stack, heap, globals, stats)
  (putStrLn . runProg) program
