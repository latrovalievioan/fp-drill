import Data.List
-- Lists:
-- - List is a listing of elements in a certain order
-- - Lists in haskell are Linked List data structure
-- - Lists only have one internal type

-- Lists (Constructors):
-- - []
-- - x: xs
-- the colon prepends the value x to an already existing list of xs
list :: [Int]
list = [1,2,3,4]

list' :: [Int]
list' = (1 : (2 : (3 : (4 : []))))

-- Lists (Generationg a list):
asc :: Int -> Int -> [Int]
asc n m
  | m < n = []
  | m == n = [m]
  | otherwise = n: asc (n + 1) m

-- Lists (Functions):
-- Most of the predefined list functions reside in the Data.List module
-- Let's check some of them:

-- Head returns the head of the list:
h = head [1,2,3] -- 1

-- Tail returns the tail of the list:
t = tail [1,2,3] -- [2,3]

-- Length returns the length of the list
l = length [1,2,3] -- 3

-- Init returns a copy of the list without the last element
i = init [1,2,3] -- [1,2]

-- Determining if a list is empty or not
isEmpty = null [] -- true
isEmpty' = null [1,2,3] -- false

-- Functions on list of Booleans:"
-- and & or perform the boolean AND or OR on any amount of booleans
-- This helps us to check a lot of conditions
testAnd = and [True, True, False, False, True]
testOr = or [True, True, False, False, True]

-- Lists (Comprehantion):
-- List comprehansions can be used to take one list or multiple lists
-- and build new lists out of them
-- [<gen> | <elem> <- <list>, ..., <guard>, ...]
isEvenList = [even x | x <- [1,2,3,4,5,6,7,8,9]]

evens = [x | x <- [1,2,3,4,5,6,7,8,9], even x]

-- Lists (Patterns):
-- In the following example, notice how we pattern match the 
-- empty list to return the int 0 since
sumList :: [Int] -> Int
sumList []     = 0
sumList (x:xs) = x + sumList xs

evens' :: [Int] -> [Int] 
evens' [] = []
evens' (x: xs)
  | even x = x: evens' xs
  | otherwise = evens' xs

-- Tuples:
-- Tuples are a way to have multiple elements in a pair
tuple :: (Int, String)
tuple = (1, "Hello")

-- Tuples can be pattern matched also
fst' :: (a, b) -> a
fst' (f, _) = f

snd' :: (a, b) -> b
snd' (_, s) = s
