module TiState where

import Debug.Trace
import Language
import PrettyPrint
import Parser
import Utils



extraPreludeDefns :: CoreProgram
extraPreludeDefns = []

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = [Addr]

type TiDump = [TiStack]
initialTiDump = []

type TiHeap = Heap Node

data Node
  = NAp Addr Addr                   -- Application
  | NSupercomb Name [Name] CoreExpr -- Supercombinator
  | NNum Int                        -- A number
  | NInd Addr                       -- Indirection
  | NPrim Name Primitive            -- Primitive
  | NData Int [Addr]                -- Tag, list of components
  deriving (Show)

data Primitive
  = Neg
  | Add
  | Sub
  | Mul
  | Div
  | PrimConstr Int Int
  | PrimCasePair
  | If
  | Greater
  | GreaterEq
  | Less
  | LessEq
  | Eq
  | NotEq
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
buildInitialHeap sc_defs
  = (heap2, sc_addrs ++ prim_addrs)
    where
      (heap1, sc_addrs) = mapAccuml allocateSc hInitial sc_defs
      (heap2, prim_addrs) = mapAccuml allocatePrim heap1 primitives

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body)
  = (heap', (name, addr))
    where
      (heap', addr) = hAlloc heap (NSupercomb name args body)

primitives :: ASSOC Name Primitive
primitives = [ ("negate", Neg)
             , ("+", Add), ("-", Sub)
             , ("*", Mul), ("/", Div)
             , ("if", If)
             , (">", Greater)
             , (">=", GreaterEq)
             , ("<", Less)
             , ("<=", LessEq)
             , ("==", Eq)
             , ("~=", NotEq)
             , ("casePair", PrimCasePair)
             ]

allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocatePrim heap (name, prim)
  = (heap', (name, addr))
    where
      (heap', addr) = hAlloc heap (NPrim name prim)



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
tiFinal ([sole_addr], [], heap, globals, stats) = isDataNode (hLookup heap sole_addr)
tiFinal ([], [], heap, globals, stats) = error "Empty stack!"
tiFinal state = False

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode (NData tag addrs) = True
isDataNode node     = False

step :: TiState -> TiState
step state
  = dispatch (hLookup heap (hd stack))
    where
      (stack, dump, heap, globals, stats) = state
      dispatch (NNum n)                  = numStep state n
      dispatch (NAp a1 a2)               = apStep state a1 a2
      dispatch (NSupercomb sc args body) = scStep state sc args body
      dispatch (NInd addr)               = scInd state addr
      dispatch (NPrim name prim)         = primStep state prim
      dispatch (NData tag addrs)         = dataStep state tag addrs

numStep :: TiState -> Int -> TiState
numStep (stack, dump, heap, globals, stats) n
  = if length dump /= 0
      then (hd dump, tl dump, heap, globals, stats)
      else error "Number applied as a function!"

-- # tumbling down to the spine
apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2
  = case node of
      (NInd a3) -> (stack, dump, new_heap, globals, stats)
                   where
                     new_heap = hUpdate heap (hd stack) (NAp a1 a3)
      otherwise -> (a1 : stack, dump, heap, globals, stats)
    where
      node = hLookup heap a2

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) sc_name arg_names body
  = if (length arg_bindings) == (length arg_names)
      then (new_stack, dump, new_heap, globals, new_stats')
      else error "Insufficient arguments"
    where
      new_stats' = tiStatIncHeap (hSize new_heap - hSize heap) new_stats
      new_stats = tiStatIncSReductions stats
      new_stack = root_addr : ss
      new_heap = instantiateAndUpdate body root_addr heap env
      (root_addr:ss) = drop (length arg_names) stack
      env = arg_bindings ++ globals
      arg_bindings = zip2 arg_names (getargs heap stack)

scInd :: TiState -> Addr -> TiState
scInd (stack, dump, heap, globals, stats) addr
  = (new_stack, dump, heap, globals, stats)
    where
      new_stack = addr : (tl stack)

primStep :: TiState -> Primitive -> TiState
primStep state Neg = primNeg state
-- for data constructors
primStep state If = primIf state
primStep state PrimCasePair = primCasePair state
primStep state (PrimConstr tag arity) = primConstr state tag arity
primStep state Add       = primDyadic state $ primArith (+)
primStep state Sub       = primDyadic state $ primArith (-)
primStep state Mul       = primDyadic state $ primArith (*)
primStep state Div       = primDyadic state $ primArith (div)
primStep state Greater   = primDyadic state $ primComp (>)
primStep state GreaterEq = primDyadic state $ primComp (>=)
primStep state Less      = primDyadic state $ primComp (<)
primStep state LessEq    = primDyadic state $ primComp (<=)
primStep state Eq        = primDyadic state $ primComp (==)
primStep state NotEq     = primDyadic state $ primComp (/=)

dataStep :: TiState -> Int -> [Addr] -> TiState
dataStep (stack, dump, heap, globals, stats) tag addrs
  = if length dump /= 0
      then (hd dump, tl dump, heap, globals, stats)
      else error "data constructor applied as a function!"

primNeg :: TiState -> TiState
primNeg (stack@(_:ss), dump, heap, globals, stats)
  = case (isDataNode node) of
      -- the result may be a indirection, so we have to step back instead of
      -- passing the whole stack
      False -> ([addr], ss:dump, heap, globals, stats)
      True -> (ss, dump, new_heap, globals, stats)
              where
                new_heap = hUpdate heap root_addr (NNum (negate n))
                root_addr = hd ss
                (NNum n) = node
    where
      node = hLookup heap addr
      [addr] = getargs heap stack

primIf :: TiState -> TiState
primIf (stack@(_:_:_:ss), dump, heap, globals, stats)
  = case (isDataNode node1) of
      False -> ([addr1], ss:dump, heap, globals, stats)
      True  -> (ss, dump, new_heap, globals, stats)
               where
                 new_heap = case tag == 2 of
                              True  -> hUpdate heap root_addr (NInd addr2)
                              False -> hUpdate heap root_addr (NInd addr3)
                 root_addr = hd ss
                 (NData tag addrs) = node1
    where
      node3 = hLookup heap addr3
      node2 = hLookup heap addr2
      node1 = hLookup heap addr1
      [addr1, addr2, addr3] = getargs heap stack

primConstr :: TiState -> Int -> Int -> TiState
primConstr (stack, dump, heap, globals, stats) tag arity
  = (new_stack, dump, new_heap, globals, stats)
    where
      new_heap = hUpdate heap addr (NData tag (take arity addrs))
      new_stack@(addr:_) = drop arity stack
      addrs = getargs heap stack

primCasePair :: TiState -> TiState
primCasePair (stack@(_:s:ss), dump, heap, globals, stats)
  = case (isDataNode node1) of
      False -> ([addr1], (s:ss):dump, heap, globals, stats)
      True  -> (ss, dump, new_heap', globals, stats)
               where
                 new_heap' = hUpdate new_heap root_addr (NInd addr)
                 root_addr = hd ss
                 (new_heap, addr) = nodeApply (heap, addr2) addrs
                 (NData tag addrs) = node1
    where
      node1 = hLookup heap addr1
      [addr1, addr2] = getargs heap stack

nodeApply :: (TiHeap, Addr) -> [Addr] -> (TiHeap, Addr)
nodeApply (heap, f) [] = (heap, f)
nodeApply (heap, f) (a:as) = nodeApply (hAlloc heap (NAp f a)) as

primDyadic :: TiState -> (Node -> Node -> Node) -> TiState
primDyadic (stack@(_:s:ss), dump, heap, globals, stats) comp
  = case (isDataNode node1) of
      False -> ([addr1], (s:ss):dump, heap, globals, stats)
      True  -> case (isDataNode node2) of
                 False -> ([addr2], ss:dump, heap, globals, stats)
                 True  -> ([root_addr], dump, new_heap, globals, stats)
                          where
                            new_heap = hUpdate heap root_addr (comp node1 node2)
                            root_addr = hd ss
    where
      node2 = hLookup heap addr2
      node1 = hLookup heap addr1
      [addr1, addr2] = getargs heap stack

primArith :: (Int -> Int -> Int) -> Node -> Node -> Node
primArith f (NNum a) (NNum b) = (NNum (f a b))
primArith _ _ _ = error "A non-numerical data is compared to another node."

primComp :: (Int -> Int -> Bool) -> Node -> Node -> Node
primComp comp (NNum a) (NNum b) = boolToNData (comp a b)
primComp comp (NData a _) (NData b _) = boolToNData (comp a b) -- buggy
primComp comp (NNum _) (NData _ _) = error "A number is compared to a data constructor."
primComp comp (NData _ _) (NNum _) = error "A data constructor is compared to a number."
primComp _ _ _ = error "A non-data node is compared to another node."

boolToNData :: Bool -> Node
boolToNData True  = NData 2 []
boolToNData False = NData 1 []



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
  = hAlloc heap (NPrim ("Pack{" ++ show tag ++ "," ++ show arity ++ "}") (PrimConstr tag arity))
instantiateLet isrec defs body heap env
  = instantiate body new_heap new_env
    where
      (new_heap, addrs) = mapAccuml (instantiateDefs isrec) heap exprs
      new_env = (zip2 names addrs) ++ env
      instantiateDefs True heap expr = instantiate expr heap new_env
      instantiateDefs False heap expr = instantiate expr heap env
      exprs = aRange defs
      names = aDomain defs

instantiateAndUpdate :: CoreExpr -> Addr -> TiHeap -> ASSOC Name Addr -> TiHeap
instantiateAndUpdate (ENum n) upd_addr heap env
  = hUpdate heap upd_addr (NNum n)
instantiateAndUpdate (EAp e1 e2) upd_addr heap env
  = hUpdate heap2 upd_addr (NAp a1 a2)
    where
      (heap1, a1) = instantiate e1 heap  env
      (heap2, a2) = instantiate e2 heap1 env
instantiateAndUpdate (EVar v) upd_addr heap env
  = hUpdate heap upd_addr (NInd addr)
    where
      addr = aLookup env v (error ("Undefined name " ++ show v))
instantiateAndUpdate (EConstr tag arity) upd_addr heap env
  = hUpdate heap upd_addr (NPrim ("Pack{" ++ show tag ++ "," ++ show arity ++ "}") (PrimConstr tag arity))
instantiateAndUpdate (ELet isrec defs body) upd_addr heap env
  = hUpdate new_heap' upd_addr (NInd addr)
    where
      (new_heap', addr) = instantiate body new_heap new_env
      (new_heap, addrs) = mapAccuml (instantiateDefs isrec) heap exprs
      new_env = (zip2 names addrs) ++ env
      instantiateDefs True heap expr = instantiate expr heap new_env
      instantiateDefs False heap expr = instantiate expr heap env
      exprs = aRange defs
      names = aDomain defs
instantiateAndUpdate (ECase e alts) upd_addr heap env
  = error "Can't instantiate case exprs"



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
showNode (NInd addr) = (iStr "NInd ") `iAppend` (iStr $ show addr)
showNode (NPrim name prim) = iStr ("NPrim " ++ name)
showNode (NData tag addrs)
  = iConcat
      [ iStr "Pack{"
      , iStr $ show tag
      , iStr ","
      , iStr $ show (length addrs)
      , iStr "}"
      ]

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
