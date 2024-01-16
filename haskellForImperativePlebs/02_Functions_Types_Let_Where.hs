-- Function (Definition):
-- name arg1 arg2 ... argN = <expr>

-- Function (Application):
-- name arg1 arg2 ... argN

-- Function (Examples):
-- Notice how we don't have return statements
-- our function returns the expression
inRange min max x = 
  x >= min && x <= max

t = inRange 2 4 3 -- True
f = inRange 2 4 62 -- False

-- Type (Basics):
-- name :: <type>
-- - Every value in haskell has a type and that type is strict
-- Examples:
x :: Integer
x = 1

y :: Bool
y = True

z :: Float
z = 3.14

-- Types (Functions):
-- Functions have their own types since functions are just values
inRange :: Int -> Int -> Int -> Bool

-- Functions (Let):
-- Sometimes we want to be able to save the result of some expression
-- in a variable and then return an expression using that variable
-- we can do this with "let" bindings:
inRange' :: Int -> Int -> Int -> Bool
inRange' min max x =
  let inLowerBound = min <= x
      inUpperBound = max >= x
  in
  inLowerBound && inUpperBound

-- Another way of doing this is with the "where" binding
inRange_ :: Int -> Int -> Int -> Bool
inRange_ min max x = inLowerBound && inUpperBound
  where
    inLowerBound = min <= x
    inUpperBound = max >= x

-- Functions (If):
-- We can control the flow of our program with if then else statement like so:
_inRange :: Int -> Int -> Int -> Bool
_inRange min max x = if inLowerBound then inUpperBound else False
  where
    inLowerBound = min <= x
    inUpperBound = max >= x

-- Functions (Infix):
-- Functions can be written in an infix fashion where you write the function
-- between the arguments (most operators):
sum' :: Int -> Int -> Int
sum' x y = x + y
res = sum' 5 4
res' = 5 `sum'` 4
