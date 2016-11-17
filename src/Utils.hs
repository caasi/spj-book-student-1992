module Utils where

shownum n = show n

hd :: [a] -> a
hd = head

tl :: [a] -> [a]
tl = tail

zip2 :: [a] -> [b] -> [(a, b)]
zip2 = zip


-- TODO:
-- A.1 The heap type
-- A.1.1 Specification
-- A.1.2 Representation
-- A.2 The association list type
-- A.3 Generating unique names
-- A.3.1 Representation
-- A.4 Sets
-- A.4.1 Representation
-- A.5 Other useful function definitions
