-- Magma is a set (a) of values that is Closed under a Binary Operator (.) Concat
class Magma a where
  concat' :: a -> a -> a

instance Magma Int where
  concat' = (-)

subtract' :: Int -> Int -> Int
subtract' = concat'
-- Notice how subtraction is not associative:
-- (2 - 2) + 2 != 2 - (2 + 2)

-- Semigroup is a Magma in which the Binary Operator is Associative
-- Associativity means that the order in which we apply the binary operator
-- is not important
-- Semigroup gives us a way to distribute the work between workers, processes, etc...
-- TODO: find a way to check for associativity in the semigroup
class Magma a => Semigroup' a where
  concat'' :: a -> a -> a

instance Semigroup' Int where
  concat'' = (+)

add' :: Int -> Int -> Int
add' = concat''
-- Notice how addition is associative:
-- (2 + 2) + 2 == 2 + (2 + 2)

-- Monoids are a Semigroup which contain an Neutral Element (empty)
class Semigroup' a => Monoid' a where
  empty :: a

instance Monoid' Int where
  empty = 0

-- Neutral Element acts like an Identity for Binary Operators
-- x . empty = x
-- empty . x = x
-- empty . empty = empty
--
-- Examples:
-- Addition: - empty is 0
-- 1 + 0 = 1
-- 0 + 1 = 1
-- 0 + 0 = 0
--
-- Multiplication: - empty is 1
-- 2 * 1 = 2
-- 1 * 2 = 2
-- 1 * 1 = 1
--
-- Arrays: - empty is []
-- 1 + [] = [1]
-- [] + 1 = [1]
-- [] + [] = []
--
-- TODO: Find better way to define mempty (now it is right only for concat with subtraction or add)

-- Groups are monoids in which each value has a unique inverse
-- a * a' = ID
-- a' * a = ID
class Monoid' a => Group' a where
  inverse :: a -> a

instance Group' Int where
  inverse a = -a
