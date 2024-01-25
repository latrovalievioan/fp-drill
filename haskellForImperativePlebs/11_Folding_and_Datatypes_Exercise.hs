import Data.List
import Debug.Trace

dbg :: Show a => a -> a
dbg x = trace (show x) x

-- 1. Create a function that reverses a list.

rev :: [a] -> [a]
rev = foldl' (\acc curr -> curr:acc) []

-- 2. Create a function that returns all the prefixes
--    of a given list.
prefixes :: [a] -> [[a]]
prefixes = foldl' step []
  where
    step [] curr = [[curr]]
    step acc curr = acc ++ [last acc ++ [curr]]

-- 3. Create a function that folds the elements
--    of a tree in a preorder traversal.
--    TODO: Fix this

data Tree a = Leaf a | Node a [Tree a]

prefixTree = 
  Node 'c' [
    Node 'a' [
      Leaf 'r',
      Leaf 't'
    ],
    Node 'o' [
      Node 'o' [
        Leaf 'l'
      ]
    ]
  ]

traverse' :: Tree Char -> String -> [String]
traverse' (Leaf ch) acc = [acc ++ [ch]]
traverse' (Node ch xs) acc = flatMap (\x -> 'g') xs
