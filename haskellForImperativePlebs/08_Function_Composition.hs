-- For function composition in Haskell we use the "." operator.
-- (.) :: (b -> c) -> (a -> b) -> a -> c
--
-- (f . g) == (\x -> f (g x))
--
-- Lets define a function that takes a function
-- then returns a function that receives a 2d list
-- then it applies the first function it received to each cell
-- and returns a list
map2d :: (a -> b) -> [[a]] -> [[b]]
-- we can define it like so:
map2d = map . map

-- let's brak down what's happening:
-- Map usage:
-- map (\x -> x + 1) [1,2,3]
-- We can write it as an anonymous function like so:
-- (\f1 xs -> map f1 xs)

-- This means that we can define map2d the following way:
map2d_ :: (a -> b) -> [[a]] -> [[b]]
map2d_ = (\f xs -> map f xs) . (\f xs -> map f xs)

-- Further more we can write the composition without the dot
map2d__ :: (a -> b) -> [[a]] -> [[b]]
map2d__ = (\fn -> (\f xs -> map f xs) (((\f xs -> map f xs) fn)))

-- We can rewrite our map2d function by making it take params instead of using lambdas:
map2d' :: (a -> b) -> [[a]] -> [[b]]
map2d' fn = map (\row -> map fn row)

-- Another way"
map2d'' :: (a -> b) -> [[a]] -> [[b]]
map2d'' fn = map (map fn)



