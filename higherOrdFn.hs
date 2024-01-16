-- Higher order function is a function that:
-- - Takes one or more functions as params or
-- - Returns a function or
-- - Does both

filter' :: (a -> Bool) -> [a] -> [a]
filter' fn [] = []
filter' fn (x: xs)
  | fn x = x: filter' fn xs
  | otherwise = filter' fn xs

evens :: [Int] -> [Int]
evens = filter' even
