-- Recursion:
-- Recursion is the way to perform loops in FP
-- name <args> ... name <args'> ...

factorial :: Int -> Int
factorial x = 
  if x <= 1
    then 1
  else 
    x * factorial (x - 1)

-- Guards:
-- There is an another way to branch our program
-- The good thing about guards is that we can create more than 2 branches with them.
-- "otherwise" always evaluates to true
factorial' :: Int -> Int
factorial' x
  | x <= 1 = 1
  | otherwise = x * factorial' (x - 1)

-- Pattern Matching:
-- We can also use pattern matching to branch our program like so:
-- Notice the underscore, it is not just a name for a variable, but it is a wildcard
isZero :: Int -> Bool
isZero 0 = True
isZero _ = False

-- Accumulators:
-- Another way of calculating factorial is using accumulators:
factorial_ :: Int -> Int
factorial_ x = aux x 1
  where
    aux :: Int -> Int -> Int
    aux x acc
      | x <= 1 = acc
      | otherwise = aux (x - 1) (x * acc)
