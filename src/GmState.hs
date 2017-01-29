module GmState where

import Debug.Trace
import Language
import PrettyPrint
import Parser
import Utils



runProg :: String -> String
runProg = showResults . eval . compile . parse



type GmState =
  ( GmCode
  , GmStack
  , GmHeap
  , GmGlobals
  , GmStats
  )

type GmCode = [Instruction]

getCode :: GmState -> GmCode
getCode (i, stack, heap, globals, stats) = i

putCode :: GmCode -> GmState -> GmState
putCode i' (i, stack, heap, globals, stats)
  = (i', stack, heap, globals, stats)

data Instruction
  = Unwind
  | Pushglobal Name
  | Pushint Int
  | Push Int
  | Mkap
  | Slide Int
  deriving (Eq)

type GmStack = [Addr]

getStack :: GmState -> GmStack
getStack (i, stack, heap, globals, stats) = stack

putStack :: GmStack -> GmState -> GmState
putStack stack' (i, stack, heap, globals, stats)
  = (i, stack', heap, globals, stats)

type GmHeap = Heap Node

getHeap :: GmState -> GmHeap
getHeap (i, stack, heap, globals, stats) = heap

putHeap :: GmHeap -> GmState -> GmState
putHeap heap' (i, stack, heap, globals, stats)
  = (i, stack, heap', globals, stats)

data Node
  = NNum Int              -- Numbers
  | NAp Addr Addr         -- Applicatons
  | NGlobal Int GmCode    -- Globals

type GmGlobals = ASSOC Name Addr

getGlobals :: GmState -> GmGlobals
getGlobals (i, stack, heap, globals, stats) = globals

type GmStats = Int

statInitial :: GmStats
statInitial = 0

statIncSteps :: GmStats -> GmStats
statIncSteps s = s + 1

statGetSteps :: GmStats -> Int
statGetSteps s = s

getStats :: GmState -> GmStats
getStats (i, stack, heap, globals, stats) = stats

putStats :: GmStats -> GmState -> GmState
putStats stats' (i, stack, heap, globals, stats)
  = (i, stack, heap, globals, stats')




showResults :: [GmState] -> String
showResults = undefined

compile :: CoreProgram -> GmState
compile = undefined

eval :: GmState -> [GmState]
eval = undefined
