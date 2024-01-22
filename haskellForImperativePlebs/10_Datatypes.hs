-- data Name =
--  Constructor1 <args> | Constructor2 <args> | ...
-- Examples:

data Color =
  Red | Orange | Yellow | Green | Blue

data Calculation =
  Add Int Int | Sub Int Int | Mult Int Int | Div Int Int

calc :: Calculation -> Int
calc (Add x y) = x + y
calc (Sub x y) = x - y
calc (Mult x y) = x * y
calc (Div x y) = div x y
