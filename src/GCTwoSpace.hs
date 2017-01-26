module GCTwoSpace (gc) where

import Language
import TiTypes
import Utils



gc :: TiState -> TiState
gc state@(output, stack, dump, heap, globals, stats)
  = (output, stack', dump', heap', globals', stats)
    where
      heap' = scavengeHeap fromheap'' toheap''
      (fromheap'', toheap'', globals') = evacuateGlobals fromheap' toheap' globals
      (fromheap', toheap', dump') = evacuateDump fromheap toheap dump
      (fromheap, toheap, stack') = evacuateStack heap hInitial stack

evacuateStack :: TiHeap -> TiHeap -> TiStack -> (TiHeap, TiHeap, TiStack)
evacuateStack = evacuateAddrs

evacuateDump :: TiHeap -> TiHeap -> TiDump -> (TiHeap, TiHeap, TiDump)
evacuateDump
  = go []
    where
      go acc fromheap toheap [] = (fromheap, toheap, reverse acc)
      go acc fromheap toheap (d:ds) = go (d':acc) f t ds
                                      where
                                        (f, t, d') = evacuateAddrs fromheap toheap d

evacuateGlobals :: TiHeap -> TiHeap -> TiGlobals -> (TiHeap, TiHeap, TiGlobals)
evacuateGlobals fromheap toheap globals
  = (fromheap', toheap', zip2 domain addrs')
    where
      (fromheap', toheap', addrs') = evacuateAddrs fromheap toheap addrs
      domain = aDomain globals
      addrs = aRange globals

evacuateAddrs :: TiHeap -> TiHeap -> [Addr] -> (TiHeap, TiHeap, [Addr])
evacuateAddrs
  = go []
    where
      go acc fromheap toheap [] = (fromheap, toheap, reverse acc)
      go acc fromheap toheap (a:as) = go (a':acc) f t as
                                      where
                                        (f, t, a') = evacuate fromheap toheap a

evacuate :: TiHeap -> TiHeap -> Addr -> (TiHeap, TiHeap, Addr)
evacuate fromheap toheap addr
  = case node of
      NForward addr1  -> (fromheap, toheap, addr1)
      NAp addr1 addr2 -> (fromheap'', toheap'', toaddr)
                         where
                           (fromheap'', toheap'', _) = evacuateAddrs fromheap' toheap' [addr1, addr2]
      NInd addr1      -> (fromheap'', toheap'', toaddr)
                         where
                           (fromheap'', toheap'', _) = evacuate fromheap' toheap' addr1
      NData _ addrs   -> (fromheap'', toheap'', toaddr)
                         where
                           (fromheap'', toheap'', _) = evacuateAddrs fromheap' toheap' addrs
      _               -> (fromheap', toheap', toaddr)
    where
      fromheap' = hUpdate fromheap addr (NForward toaddr)
      (toheap', toaddr) = hAlloc toheap node
      node = hLookup fromheap addr

scavengeHeap :: TiHeap -> TiHeap -> TiHeap
scavengeHeap fromheap toheap
  = toheap'
    where
      (toheap', _) = mapAccuml (scavengeAddr fromheap) toheap addrs
      addrs = hAddresses toheap

scavengeAddr :: TiHeap -> TiHeap -> Addr -> (TiHeap, Addr)
scavengeAddr fromheap toheap addr
  = case node of
      NAp addr1 addr2 -> (hUpdate toheap addr (NAp addr1' addr2'), addr)
                         where
                           addr2' = toNewAddr fromheap addr2
                           addr1' = toNewAddr fromheap addr1
      -- how can I remove the indirection node here?
      NInd addr1      -> (hUpdate toheap addr (NInd addr1'), addr)
                         where
                           addr1' = toNewAddr fromheap addr1
      NData tag addrs -> (hUpdate toheap addr (NData tag addrs'), addr)
                         where
                           addrs' = map (toNewAddr fromheap) addrs
      _               -> (toheap, addr)
    where
      toNewAddr heap = (\(NForward addr) -> addr) . (hLookup heap)
      node = hLookup toheap addr

