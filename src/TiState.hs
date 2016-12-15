module TiState where

import Language
import PrettyPrint
import Parser
import Utils



extraPreludeDefns :: CoreProgram
extraPreludeDefns = []

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = [Addr]

data TiDump = DummyTiDump
  deriving (Show)
initialTiDump = DummyTiDump

type TiHeap = Heap Node

data Node
  = NAp Addr Addr                   -- Application
  | NSupercomb Name [Name] CoreExpr -- Supercombinator
  | NNum Int                        -- A number
  deriving (Show)

type TiGlobals = ASSOC Name Addr

type TiStats = (Int, Int, Int, Int, Int)

tiStatInitial :: TiStats
tiStatInitial = (0, 0, 0, 0, 0)

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps (s, sr, pr, h, d) = (s + 1, sr, pr, h, d)

tiStatIncSReductions :: TiStats -> TiStats
tiStatIncSReductions (s, sr, pr, h, d) = (s, sr + 1, pr, h, d)

tiStatIncPReductions :: TiStats -> TiStats
tiStatIncPReductions (s, sr, pr, h, d) = (s, sr, pr + 1, h, d)

tiStatIncHeap :: Int -> TiStats -> TiStats
tiStatIncHeap n (s, sr, pr, h, d) = (s, sr, pr, h + n, d)

tiStatSetMaxDepth :: Int -> TiStats -> TiStats
tiStatSetMaxDepth n stat@(s, sr, pr, h, d)
  = if n > d
      then (s, sr, pr, h, n)
      else stat

tiStatGetSteps :: TiStats -> Int
tiStatGetSteps (s, sr, pr, h, d) = s

tiStatGetSReductions :: TiStats -> Int
tiStatGetSReductions (s, sr, pr, h, d) = sr

tiStatGetPReductions :: TiStats -> Int
tiStatGetPReductions (s, sr, pr, h, d) = pr

tiStatGetHeap :: TiStats -> Int
tiStatGetHeap (s, sr, pr, h, d) = h

tiStatGetDepth :: TiStats -> Int
tiStatGetDepth (s, sr, pr, h, d) = d

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
doAdmin state
  = state''
    where
      state'' = applyToStats (tiStatSetMaxDepth (length stack)) state'
      state'@(stack, dump, heap, glabals, stats) = applyToStats tiStatIncSteps state

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
  = if (length arg_bindings) == (length arg_names)
      then (new_stack, dump, new_heap, globals, new_stats')
      else error "Insufficient arguments"
    where
      new_stats' = tiStatIncHeap (hSize new_heap - hSize heap) new_stats
      new_stats = tiStatIncSReductions stats
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
instantiate (ENum n) heap env = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env
  = hAlloc heap2 (NAp a1 a2)
    where
      (heap1, a1) = instantiate e1 heap  env
      (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env
  = (heap, aLookup env v (error ("Undefined name " ++ show v)))
instantiate (EConstr tag arity) heap env
  = instantiateConstr tag arity heap env
instantiate (ELet isrec defs body) heap env
  = instantiateLet isrec defs body heap env
instantiate (ECase e alts) heap env = error "Can't instantiate case exprs"

instantiateConstr tag arity heap env
  = error "Can't instantiate constructors yet"
instantiateLet isrec defs body heap env
  = error "Can't instantiate let(rec)s yet"



showResults :: [TiState] -> String
showResults states
 = iDisplay (iConcat [ iLayn (map showState states), showStats (last states) ])

showState :: TiState -> Iseq
showState (stack, dump, heap, globals, stats)
  = iConcat [ showStack heap stack, iNewline, showHeap heap, iNewline ]

showStack :: TiHeap -> TiStack -> Iseq
showStack heap stack
  = iConcat
      [ iStr "Stk  ["
      , iIndent (iInterleave iNewline (map show_stack_item stack))
      , iStr " ]"
      ]
    where
      show_stack_item addr
        = iConcat
            [ showFWAddr addr
            , iStr ": "
            , showStkNode heap (hLookup heap addr)
            ]

showStkNode :: TiHeap -> Node -> Iseq
showStkNode heap (NAp fun_addr arg_addr)
  = iConcat
      [ iStr "NAp "
      , showFWAddr fun_addr
      , iStr " "
      , showFWAddr arg_addr
      , iStr " ("
      , showNode (hLookup heap arg_addr)
      , iStr ")"
      ]
showStkNode heap node = showNode node

showNode :: Node -> Iseq
showNode (NAp a1 a2)
  = iConcat
      [ iStr "NAp "
      , showAddr a1
      , iStr " "
      , showAddr a2
      ]
showNode (NSupercomb name args body) = iStr ("NSupercomb " ++ name)
showNode (NNum n) = (iStr "NNum ") `iAppend` (iNum n)

showAddr :: Addr -> Iseq
showAddr addr = iStr (show addr)

showFWAddr :: Addr -> Iseq -- Show address in field of width 4
showFWAddr addr
  = iStr (spaces (4 - length str) ++ str)
    where
      str = show addr

showStats :: TiState -> Iseq
showStats (stack, dump, heap, globals, stats)
  = iConcat
      [ iNewline
      , iNewline
      , iStr "Total number of steps = "
      , iNum (tiStatGetSteps stats)
      , iNewline
      , iStr "Total number of supercombinator reductions = "
      , iNum (tiStatGetSReductions stats)
      , iNewline
      , iStr "Total number of primitive reductions = "
      , iNum (tiStatGetPReductions stats)
      , iNewline
      , iStr "New heaps allocated = "
      , iNum (tiStatGetHeap stats)
      , iNewline
      , iStr "Max stack depth = "
      , iNum (tiStatGetDepth stats)
      ]



showHeap :: TiHeap -> Iseq
showHeap (size, free, cts) = showASSOC cts

showASSOC :: ASSOC Addr Node -> Iseq
showASSOC pairs
  = iConcat
    [ iStr "Heap ["
    , iIndent (iInterleave iNewline (map showAddrNode pairs))
    , iStr " ]"
    ]

showAddrNode :: (Addr, Node) -> Iseq
showAddrNode (addr, node)
  = iConcat
    [ showFWAddr addr
    , iStr ": "
    , showNode node
    ]
