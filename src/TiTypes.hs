module TiTypes where

import Language
import Utils



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
  | NMarked Node                    -- Marked node
  | NForward Addr                   -- Address pointed to the to-space heap
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

