import Data.List
-- Typeinfernce is important for haskell since for every
-- expression we want to know what the most general type is.
-- (keep it as polymorphic as possible).
-- This helps us prove that our program has a sound type.


-- Typeinference algorithm
-- 1. Assign every variable a unique typevariable.
-- 2. Assign every function its type with new unique typevariables.
-- 3. For each subexpression of the expression generate equations of types.
-- 4. Resolve the equations until no further simplifications can be done.
--    Conflicting types imply a type error, otherwise the type has been inferred.

add x y z = (x + y) : z

-- 1.
-- x :: a
-- y :: b
-- z :: c

-- 2.
-- (+) :: Num d => d -> d -> d
-- (:) :: e -> [e] -> [e]

-- 3.
-- from (x + y) derive a = d and b = d
-- from (x + y) : z derive c = [e] and e = d

-- 4.
-- x :: d
-- y :: d
-- z :: [e]
-- z :: [d]
-- add :: (Num d) => d -> d -> [d] -> [d]

f x = x : x

-- 1.
-- x :: a

-- 2.
-- (:) :: b -> [b] -> [b]

-- 3.
-- from x : x derive a = b and a = [b]

-- 4.
-- b = [b]
-- Type error.
