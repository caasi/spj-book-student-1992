module TiState where

import Debug.Trace
import Language
import PrettyPrint
import Parser
import Utils



extraPreludeDefns :: CoreProgram
extraPreludeDefns
  = [ ("False", [], EConstr 1 0)
    , ("True", [], EConstr 2 0)
    , ("and", ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "y")) (EVar "False"))
    , ("or", ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "True")) (EVar "y"))
    , ("MkPair", ["a", "b"], EAp (EAp (EConstr 1 2) (EVar "a")) (EVar "b"))
    , ("fst", ["p"], EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K"))
    , ("snd", ["p"], EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K1"))
    , ("Nil", [], EConstr 1 0)
    , ("Cons", ["x", "xs"], EAp (EAp (EConstr 2 2) (EVar "x")) (EVar "xs"))
    -- head xs = caseList xs abort K
    , ("head", ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs")) (EVar "abort")) (EVar "K"))
    -- tail xs = caseList xs abort K1
    , ("tail", ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs")) (EVar "abort")) (EVar "K1"))
    , ("printList", ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs")) (EVar "stop")) (EVar "printCons"))
    , ("printCons", ["h", "t"], EAp (EAp (EVar "print") (EVar "h")) (EAp (EVar "printList") (EVar "t")))
    , ("__main", [], EAp (EVar "printList") (EVar "main"))
    ]

type TiState = ([Int], TiStack, TiDump, TiHeap, TiGlobals, TiStats)

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
  | PrimCaseList
  | If
  | Greater
  | GreaterEq
  | Less
  | LessEq
  | Eq
  | NotEq
  | Abort
  | Print
  | Stop
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
applyToStats stats_fn (output, stack, dump, heap, sc_defs, stats)
  = (output, stack, dump, heap, sc_defs, stats_fn stats)



runProg :: String -> String
runProg = showResults . eval . compile . parse



compile :: CoreProgram -> TiState
compile program
  = ([], initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
    where
      sc_defs = program ++ preludeDefns ++ extraPreludeDefns
      (initial_heap, globals) = buildInitialHeap sc_defs
      initial_stack = [address_of_main]
      address_of_main = aLookup globals "__main" (error "the impossible happened")

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
             , ("caseList", PrimCaseList)
             , ("abort", Abort)
             , ("print", Print)
             , ("stop", Stop)
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
  = state'''
    where
      state''' = if (length stack > 40) then (gc state'') else state''
      state'' = applyToStats (tiStatSetMaxDepth (length stack)) state'
      state'@(output, stack, dump, heap, glabals, stats) = applyToStats tiStatIncSteps state

tiFinal :: TiState -> Bool
tiFinal (output, [sole_addr], [], heap, globals, stats) = isDataNode (hLookup heap sole_addr)
tiFinal (output, [], [], heap, globals, stats) = True
tiFinal state = False

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode (NData tag addrs) = True
isDataNode node     = False

step :: TiState -> TiState
step state
  = dispatch (hLookup heap (hd stack))
    where
      (output, stack, dump, heap, globals, stats) = state
      dispatch (NNum n)                  = numStep state n
      dispatch (NAp a1 a2)               = apStep state a1 a2
      dispatch (NSupercomb sc args body) = scStep state sc args body
      dispatch (NInd addr)               = scInd state addr
      dispatch (NPrim name prim)         = primStep state prim
      dispatch (NData tag addrs)         = dataStep state tag addrs

numStep :: TiState -> Int -> TiState
numStep (output, stack, dump, heap, globals, stats) n
  = if length dump /= 0
      then (output, hd dump, tl dump, heap, globals, stats)
      else error "Number applied as a function!"

-- # tumbling down to the spine
apStep :: TiState -> Addr -> Addr -> TiState
apStep (output, stack, dump, heap, globals, stats) a1 a2
  = case node of
      (NInd a3) -> (output, stack, dump, new_heap, globals, stats)
                   where
                     new_heap = hUpdate heap (hd stack) (NAp a1 a3)
      otherwise -> (output, a1 : stack, dump, heap, globals, stats)
    where
      node = hLookup heap a2

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (output, stack, dump, heap, globals, stats) sc_name arg_names body
  = if (length arg_bindings) == (length arg_names)
      then (output, new_stack, dump, new_heap, globals, new_stats')
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
scInd (output, stack, dump, heap, globals, stats) addr
  = (output, new_stack, dump, heap, globals, stats)
    where
      new_stack = addr : (tl stack)

primStep :: TiState -> Primitive -> TiState
primStep state Neg = primNeg state
-- for data constructors
primStep state If = primIf state
primStep state PrimCasePair = primCasePair state
primStep state PrimCaseList = primCaseList state
primStep state (PrimConstr tag arity) = primConstr state tag arity
primStep state Abort = primAbort state
primStep state Print = primPrint state
primStep state Stop  = primStop state
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
dataStep (output, stack, dump, heap, globals, stats) tag addrs
  = if length dump /= 0
      then (output, hd dump, tl dump, heap, globals, stats)
      else error "data constructor applied as a function!"

primNeg :: TiState -> TiState
primNeg (output, stack@(_:ss), dump, heap, globals, stats)
  = case (isDataNode node) of
      -- the result may be a indirection, so we have to step back instead of
      -- passing the whole stack
      False -> (output, [addr], ss:dump, heap, globals, stats)
      True -> (output, ss, dump, new_heap, globals, stats)
              where
                new_heap = hUpdate heap root_addr (NNum (negate n))
                root_addr = hd ss
                (NNum n) = node
    where
      node = hLookup heap addr
      [addr] = getargs heap stack

primIf :: TiState -> TiState
primIf (output, stack@(_:_:_:ss), dump, heap, globals, stats)
  = case (isDataNode node1) of
      False -> (output, [addr1], ss:dump, heap, globals, stats)
      True  -> (output, ss, dump, new_heap, globals, stats)
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
primConstr (output, stack, dump, heap, globals, stats) tag arity
  = (output, new_stack, dump, new_heap, globals, stats)
    where
      new_heap = hUpdate heap addr (NData tag (take arity addrs))
      new_stack@(addr:_) = drop arity stack
      addrs = getargs heap stack

primCasePair :: TiState -> TiState
primCasePair (output, stack@(_:s:ss), dump, heap, globals, stats)
  = case (isDataNode node1) of
      False -> (output, [addr1], (s:ss):dump, heap, globals, stats)
      True  -> (output, ss, dump, new_heap', globals, stats)
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

primCaseList :: TiState -> TiState
primCaseList (output, stack@(_:s1:s2:ss), dump, heap, globals, stats)
  = case (isDataNode node1) of
      False -> (output, [addr1], (s1:s2:ss):dump, heap, globals, stats)
      True  -> case tag of
                 -- Nil
                 1 -> (output, ss, dump, new_heap, globals, stats)
                      where
                        new_heap = hUpdate heap root_addr (NInd addr2)
                 -- Cons x xs
                 2 -> (output, ss, dump, new_heap', globals, stats)
                      where
                        new_heap' = hUpdate new_heap root_addr (NInd addr)
                        -- addr3 should point to a supercombinator for now :(
                        (new_heap, addr) = nodeApply (heap, addr3) addrs
               where
                 root_addr = hd ss
                 (NData tag addrs) = node1
    where
      node1 = hLookup heap addr1
      [addr1, addr2, addr3] = getargs heap stack

primDyadic :: TiState -> (Node -> Node -> Node) -> TiState
primDyadic (output, stack@(_:s:ss), dump, heap, globals, stats) comp
  = case (isDataNode node1) of
      False -> (output, [addr1], (s:ss):dump, heap, globals, stats)
      True  -> case (isDataNode node2) of
                 False -> (output, [addr2], ss:dump, heap, globals, stats)
                 True  -> (output, ss, dump, new_heap, globals, stats)
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

primAbort :: TiState -> TiState
primAbort (output, stack, dump, heap, globals, stats)
  = error "Abort!"

primPrint :: TiState -> TiState
primPrint (output, stack@(_:s:ss), dump, heap, globals, stats)
  = case (isDataNode node1) of
      False -> (output, [addr1], (s:ss):dump, heap, globals, stats)
      True  -> (output ++ [n], [addr2], dump, heap, globals, stats)
               where
                 root_addr = hd ss
                 (NNum n) = node1
    where
      node1 = hLookup heap addr1
      [addr1, addr2] = getargs heap stack

primStop :: TiState -> TiState
primStop (output, _, [], heap, globals, stats) = (output, [], [], heap, globals, stats)
primStop (output, _, dump, heap, globals, stats) = (output, hd dump, tl dump, heap, globals, stats)



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
 = iDisplay (iConcat [ iLayn (map showState states), showStats state, showOutput state ])
   where
     state@(output, stack, dump, heap, globals, stats) = last states

showState :: TiState -> Iseq
showState (output, stack, dump, heap, globals, stats)
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
showStats (output, stack, dump, heap, globals, stats)
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
      , iNewline
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

showOutput :: TiState -> Iseq
showOutput (output, stack, dump, heap, globals, stats)
  = iConcat
    [ iStr "Output ["
    , iInterleave (iStr ", ") $ map (iStr . show) output
    , iStr "]"
    , iNewline
    ]



-- Mark-scan collection
gc :: TiState -> TiState
gc state@(output, stack, dump, heap, globals, stats)
  = (output, stack, dump, new_heap, globals, stats)
    where
      (new_heap, _) = mapAccuml (\h a -> (markFrom h a, a)) heap addrs
      addrs = (findStackRoots state) ++ (findDumpRoots state) ++ (findGlobalRoots state)

findStackRoots :: TiState -> [Addr]
findStackRoots (output, stack, dump, heap, globals, stats)
  = stack

findDumpRoots :: TiState -> [Addr]
findDumpRoots (output, stack, dump, heap, globals, stats)
  = concat dump

findGlobalRoots :: TiState -> [Addr]
findGlobalRoots (output, stack, dump, heap, globals, stats)
  = aRange globals

markFrom :: TiHeap -> Addr -> TiHeap
markFrom = undefined

scanHeap :: TiHeap -> TiHeap
scanHeap = undefined



-- Two-space garbage collection

