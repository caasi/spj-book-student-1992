module Utils where

import Data.List



spaces = flip replicate ' '

shownum n = show n

hd :: [a] -> a
hd = head

tl :: [a] -> [a]
tl = tail

zip2 :: [a] -> [b] -> [(a, b)]
zip2 = zip



-- A.1 The heap type
hInitial :: Heap a
hInitial = (0, [1..], [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, (next:free), cts) n = ((size+1, free, (Addr next, n) : cts), Addr next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (size, free, cts) a n = (size, free, (a, n) : aRemove cts a)

hFree :: Heap a -> Addr -> Heap a
hFree (size, free, cts) a@(Addr i) = (size-1, i : free, aRemove cts a)



hLookup :: Heap a -> Addr -> a
hLookup (size, free, cts) a
  = aLookup cts a (error ("can't find node " ++ show a ++ " in heap"))

hAddresses :: Heap a -> [Addr]
hAddresses (size, free, cts) = [addr | (addr, node) <- cts]

hSize :: Heap a -> Int
hSize (size, free, cts) = size



hNull :: Addr
hNull = Addr 0

hIsnull :: Addr -> Bool
hIsnull a = a == Addr 0



type Heap a = (Int, [Int], ASSOC Addr a)

newtype Addr = Addr Int deriving (Eq)
instance Show Addr where
  show (Addr a) = "#" ++ show a



-- A.2 The association list type
type ASSOC a b = [(a, b)]

aLookup :: (Eq a) => ASSOC a b -> a -> b -> b
aLookup [] k' def = def
aLookup ((k, v):bs) k' def
  | k == k' = def
  | k /= k' = aLookup bs k' def

aDomain :: ASSOC a b -> [a]
aDomain alist = [key | (key, val) <- alist]

aRange :: ASSOC a b -> [b]
aRange alist = [val | (key, val) <- alist]

aEmpty :: ASSOC a b
aEmpty = []

aRemove :: (Eq a) => ASSOC a b -> a -> ASSOC a b
aRemove as key' = go [] as where
  go acc [] = acc
  go acc (a@(key, val):as)
    | key' == key = go acc as
    | key' /= key = go (a : acc) as



-- A.3 Generating unique names
getName :: NameSupply -> [Char] -> (NameSupply, [Char])
getName name_supply prefix = (name_supply + 1, makeName prefix name_supply)

getNames :: NameSupply -> [[Char]] -> (NameSupply, [[Char]])
getNames name_supply prefixes
  = (name_supply + length prefixes, zipWith makeName prefixes [name_supply..])

initialNameSupply :: NameSupply
initialNameSupply = 0



type NameSupply = Int

makeName :: [Char] -> NameSupply -> [Char]
makeName prefix ns = prefix ++ "_" ++ show ns



-- A.4 Sets
setFromList :: (Ord a) => [a] -> Set a
setFromList = rmdup . sort where
  rmdup [] = []
  rmdup [x] = [x]
  rmdup (x:y:xs)
    | x == y = rmdup (y : xs)
    | x /= y = x : rmdup (y : xs)

setToList :: (Ord a) => Set a -> [a]
setToList xs = xs

setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion [] [] = []
setUnion [] (b:bs) = (b:bs)
setUnion (a:as) [] = (a:as)
setUnion (a:as) (b:bs)
  | a < b  = a : setUnion as (b : bs)
  | a == b = a : setUnion as bs
  | a > b  = b : setUnion (a : as) bs

setIntersection :: (Ord a) => Set a -> Set a -> Set a
setIntersection [] [] = []
setIntersection [] (b:bs) = []
setInertsection (a:as) [] = []
setInetrsection (a:as) (b:bs)
  | a < b  = setIntersection as (b : bs)
  | a == b = a : setIntersection as bs
  | a > b  = setIntersection (a : as) bs

setSubtraction :: (Ord a) => Set a -> Set a -> Set a
setSubtraction [] [] = []
setSubtraction [] (b:bs) = []
setSubtraction (a:as) (b:bs)
  | a < b  = a : setSubtraction as (b : bs)
  | a == b = setSubtraction as bs
  | a > b  = setSubtraction (a : as) bs

setElementOf :: (Ord a) => a -> Set a -> Bool
setElementOf x [] = False
setElemnteOf x (y:ys) = x == y || (x > y && setElementOf x ys)

setEmpty :: (Ord a) => Set a
setEmpty = []

setIsEmpty :: (Ord a) => Set a -> Bool
setIsEmpty = null

setSingleton :: (Ord a) => a -> Set a
setSingleton x = [x]

setUnionList :: (Ord a) => [Set a] -> Set a
setUnionList = foldl setUnion setEmpty



type Set a = [a]



-- A.5 Other useful function definitions
first :: (a, b) -> a
first (a, b) = a

second :: (a, b) -> b
second (a, b) = b

-- # Is there a (>>=) in mapAccuml?
mapAccuml :: (a -> b -> (a, c)) -- Function of accumulator and element
                                --   input list, returning new
                                --   accumulator and element of result list
             -> a               -- Initial accumulator
             -> [b]             -- Input list
             -> (a, [c])        -- Final accumulator and result list
mapAccuml f acc [] = (acc, [])
mapAccuml f acc (x:xs) = (acc2, x' : xs') where
  (acc1, x') = f acc x
  (acc2, xs') = mapAccuml f acc1 xs



