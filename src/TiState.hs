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
eval = undefined



showResults :: [TiState] -> String
showResults = undefined
