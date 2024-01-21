-- Partial function applictaion is a result of a pattern
-- called "Currying"
-- It is a principle that states the following:
-- "If we have a function that takes 3 arguments and returns one value
--  we could rewrite it as a function that takes only a single argument
--  and returns a function that expects one argument and which returns
--  a function that expexts a single argument and returns a value"
-- f1 :: a -> b -> c -> d
-- =
-- f2 :: a -> (b -> (c -> d))

add :: Int -> Int -> Int
add x y = x + y

add5 = add 5
-- Notice how we called the function add with only one param
-- so that now we have a function that expects a single Int param
-- and will add 5 to it. This is partial function application.


doubles :: [Int] -> [Int]
doubles = map (\x -> x * 2)

res = doubles [1,2,3,4,5,6,7,8,9]
-- Notice how we've partially applied map, so that now we have the
-- function doubles which is map bound with the function that we passed
