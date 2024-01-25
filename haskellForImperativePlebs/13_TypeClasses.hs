-- When we have a function line "+" we would want it to
-- be polymorphic so that we can use it on Floats, Ints, and
-- whatever other numeric types there are.
(<+>) :: a -> a -> a
(<+>) = undefined
-- Making our "+" function tottaly polymorphic however isnt a
-- good idea, because this means that we can use it on all types
-- but what does it even mean to for example sum lists?
-- To solve this we can use a type constrain:
(<++>) :: Num a => a -> a -> a
(<++>) = undefined
-- The "Num a =>" tells use that this function works for
-- any polymorphic type that has an instance of the Num typeclass

-- A typeclass defines some functions that need to be defined in a class
-- and for a type to be in that typeclass it needs to have an instance
-- of such class
-- :info <typeclass> in GHCI

-- Let's create instance of a typeclass
data Temperature = C Float | F Float

instance Eq Temperature where
  (==) (C n) (C m) = n == m
  (==) (F n) (F m) = n == m
  (==) (C c) (F f) = (c * 1.8 + 32) == f
  (==) (F f) (C c) = (c * 1.8 + 32) == f

-- Deriving Typeclasses
data Temperature' = C' Float | F' Float deriving (Show, Eq)
-- Deriving sometimes can be bad, in the above example the derived Eq looks like this:
--  (==) (C n) (C m) = n == m
--  (==) (F n) (F m) = n == m
--  (==) _ _ = False

