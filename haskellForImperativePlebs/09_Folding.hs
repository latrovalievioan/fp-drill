-- The following function takes one function as its argument
-- this function has 2 arguments and returns a value of the type
-- of the second argument
-- After that our function takes an argument "b" and a list of type "a"
-- and returns something of the type "b"
-- foldr :: (a -> b -> b) -> b -> [a] -> b

-- Example usage:
sum' xs = foldr (+) 0 xs
-- Note that because we are using foldr, this is evaluated the following way:
-- 0 + 4 + 3 + 2 + 1

countEvens xs = foldr (\curr acc -> if even curr then acc + 1 else acc) 0 xs
