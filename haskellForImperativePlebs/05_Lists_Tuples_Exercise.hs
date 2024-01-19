-- 1. Create a function that returns True
--    if an element is in a given list and returns
--    False otherwise
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' el (x:xs)
  | el == x = True
  | otherwise = elem' el xs

-- 2. Create a function that removes all duplicates from a list
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x: xs)
  | elem' x xs = nub' xs
  | otherwise = x: nub' xs

-- 3. Create a function that returns True if the list given
--    is a list of ascending order
isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x:y:xs)
  | x < y = isAsc $ y:xs
  | otherwise = False

-- 4. Create a function that determines if a path from one node
--    to another exists within a directed graph
--    (Graph example)
--    1 -> 2 <-> 3 <- 4 -> 5
--    TODO!

type GraphNode = (Int, Int)
type DirectedGraph = [GraphNode]
type Visited = [GraphNode]
type From = Int
type To = Int

hasPath :: DirectedGraph -> From -> To -> Bool
hasPath [] from to = from == to
hasPath (head: tail) from to
  | fst head == from = undefined
  | otherwise = hasPath tail from to

trackPath :: DirectedGraph ->  Visited -> GraphNode -> To -> Bool
trackPath graph visited currentNode to
  | elem' currentNode visited = False
  | 
