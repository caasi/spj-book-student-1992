module Main where

import Language
import PrettyPrint
import Parser
import TiState
import Utils



main :: IO ()
main = do
  --program <- getContents
  --(putStrLn . show) (clex 0 program)
  --(putStrLn . show) (parse program)
  --let coreProgram = parse program
  --let states@(stack, dump, (_, _, heap), globals, stats) = step $ step $ step $ step $ compile coreProgram
  --(putStrLn . show) $ (stack, heap, globals, stats)
  getContents >>= (putStrLn . runProg)
