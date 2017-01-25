module GCMarkScan (gc) where

import Language
import TiTypes
import Utils



gc :: TiState -> TiState
gc state@(output, stack, dump, heap, globals, stats)
  = (output, stack, dump, heap'', globals, stats)
    where
      heap'' = scanHeap heap'
      (heap', _) = mapAccuml (\h a -> (markFrom h a, a)) heap addrs
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
markFrom heap addr
  = case node of
      NAp addr1 addr2 -> hUpdate heap'' addr (NMarked node)
                         where
                           heap'' = markFrom heap' addr2
                           heap' = markFrom heap addr1
      NInd addr1      -> hUpdate heap' addr node''
                         where
                           heap' = markFrom heap addr1
                           node'' = case node' of
                                      (NMarked _) -> node'
                                      _           -> (NMarked node')
                           node' = hLookup heap addr1
      NData _ addrs   -> hUpdate heap' addr (NMarked node)
                         where
                           (heap', _) = mapAccuml (\h a -> (markFrom h a, a)) heap addrs
      NMarked _       -> heap
      _               -> hUpdate heap addr (NMarked node)
    where
      node = hLookup heap addr

scanHeap :: TiHeap -> TiHeap
scanHeap heap
  = go heap addrs
    where
      go heap []     = heap
      go heap (a:as) = case node of
                         NMarked node' -> go (hUpdate heap a node') as
                         _             -> go (hFree heap a) as
                         --_             -> go heap as -- do nothing
                       where
                         node = hLookup heap a
      addrs = hAddresses heap

