module MultState where

import Utils



type MultState = (Int, Int, Int, Int)

evalMult :: MultState -> [MultState]
evalMult state = if multFinal state
                   then [state]
                   else state : evalMult (stepMult state)

stepMult :: MultState -> MultState
stepMult (n, m, d, t)
  | d > 0  = (n, m, d - 1, t + 1)
  | d == 0 = (n, m - 1, n, t)

multFinal :: MultState -> Bool
multFinal (n, m, d, t) = m == 0 && d == 0

-- from yale-haskell
layn :: [String] -> String
layn = concat . zipWith f [1..]
         where
         f :: Int -> String -> String
         f n x = rjustify 4 (show n) ++ ") " ++ x ++ "\n"

rjustify :: Int -> String -> String
rjustify n s = spaces (n - length s) ++ s
