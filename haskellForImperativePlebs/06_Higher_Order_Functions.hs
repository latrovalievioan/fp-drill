-- Higher order functions
-- A higher order function is a function that takes a function as
-- an argument e.g:

apply :: (a -> b) -> a -> b
apply f x = f x

add1 :: Int -> Int
add1 x = x + 1

val = apply add1 5

-- Anonymous Functions:
-- Anonymous functions are an important part of higher order functions
-- because we do not need to define functions with a name every time
-- that we need a function as a param.
-- (\<args> -> <expr>)
add1' = (\x -> x + 1)

val' = apply (\x -> x + 10)

-- Examples passing lambda functions to map and filter:
ex' = map (\(x,y) -> x + y) [(1,2), (3,2), (10,20), (30, 12)]

evens = filter (\x -> even x) [1,2,3,4,5,6,7,8,9]
