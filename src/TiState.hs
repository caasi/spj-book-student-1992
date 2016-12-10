module TiState where

import Language
import Parser
import Utils



extraPreludeDefns :: CoreProgram
extraPreludeDefns = []

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = [Addr]

data TiDump = DummyTiDump
initialTiDump = DummyTiDump

type TiHeap = Heap Node

data Node
  = NAp Addr Addr                   -- Application
  | NSupercomb Name [Name] CoreExpr -- Supercombinator
  | NNum Int                        -- A number

type TiGlobals = ASSOC Name Addr

type TiStats = Int

tiStatInitial :: TiStats
tiStatInitial = 0

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps s = s + 1

tiStatGetSteps :: TiStats -> Int
tiStatGetSteps s = s

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats stats_fn (stack, dump, heap, sc_defs, stats)
  = (stack, dump, heap, sc_defs, stats_fn stats)



runProg :: String -> String
runProg = showResults . eval . compile . parse



compile :: CoreProgram -> TiState
compile program
  = (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
    where
      sc_defs = program ++ preludeDefns ++ extraPreludeDefns
      (initial_heap, globals) = buildInitialHeap sc_defs
      initial_stack = [address_of_main]
      address_of_main = aLookup globals "main" (error "main is not defined")

buildInitialHeap :: CoreProgram -> (TiHeap, TiGlobals)
buildInitialHeap sc_defs = mapAccuml allocateSc hInitial sc_defs

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body)
  = (heap', (name, addr))
    where
      (heap', addr) = hAlloc heap (NSupercomb name args body)



eval :: TiState -> [TiState]
eval state = state : rest_states
       where
         rest_states
           | tiFinal state = [] -- # so I can put a `#` in front of my guard?
           | otherwise     = eval next_states
         next_states = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state

tiFinal :: TiState -> Bool
tiFinal ([sole_addr], dump, heap, globals, stats) = isDataNode (hLookup heap sole_addr)
tiFinal ([], dump, heap, globals, stats) = error "Empty stack!"
tiFinal state = False

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode node     = False

step :: TiState -> TiState
step state
  = dispatch (hLookup heap (hd stack))
    where
      (stack, dump, heap, globals, stats) = state
      dispatch (NNum n)                  = numStep state n
      dispatch (NAp a1 a2)               = apStep state a1 a2
      dispatch (NSupercomb sc args body) = scStep state sc args body

numStep :: TiState -> Int -> TiState
numStep state n = error "Number applied as a function!"

-- # tumbling down to the spine
apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2
  = (a1 : stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) sc_name arg_names body
  = (new_stack, dump, heap, globals, stats)
    where
      new_stack = result_addr : (drop (length arg_names + 1) stack)
      (new_heap, result_addr) = instantiate body heap env
      env = arg_bindings ++ globals
      arg_bindings = zip2 arg_names (getargs heap stack)

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc : stack)
  = map get_arg stack
    -- # take the right branch as the argument
    where get_arg addr = arg where (NAp fun arg) = hLookup heap addr

instantiate :: CoreExpr -> TiHeap -> ASSOC Name Addr -> (TiHeap, Addr)
instantiate = undefined



showResults :: [TiState] -> String
showResults = undefined
