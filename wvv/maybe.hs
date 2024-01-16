 -- - Total Function
 -- Total Functions are functions that map every input to a output
 -- Example: Increment
increment :: Int -> Int
increment = (+ 1)
 --
 -- - Partial Function
 -- Partial Functions are functions that for at least one input there isn't an output
 -- Example Division (Cannot divide by zero) '*** Exception: divide by zero'
div2 :: Int -> Int
div2 = div 2
 --
 -- In FP we want all our funtions to be Total
 --
 -- If we try to compose both functions such as that div2 is called before increment,
 -- notice that if we pass 0 to composed it will throw an exception '*** Exception: divide by zero'
div2ThenIncrement = increment . div2
 --
 -- One way to make the division function Total is to consider null as some number type
 -- This will give us a way to compose division with multiplication
 -- Example: Note - 2/0 will produce "null"
 -- increment (div 2 0) 
 --
 -- This means that we'll need to consider how to handle null every time we
 -- create a new functionality for numbers
 --
 -- The Solution:
 -- Null is ok by itself, we use it to represent the absence of a value
 --
 -- Let's create a type that contains all the int values + the null value
data Option a = Some a | None deriving Show
type MaybeInt = Option Int

intToMaybeInt :: Int -> MaybeInt
intToMaybeInt a = Some a

-- Now lets try to compose both functions again but this time we don't want to get
-- an exception when deviding by 0 
-- We would need a different definition for increment and div2 that returns an Option
increment_ :: MaybeInt -> MaybeInt
increment_ None = None
increment_ (Some x) = Some (increment x)
--
div2_ :: Int -> MaybeInt
div2_ 0 = None
div2_ x = Some x

-- Now we can compose div2_ with increment the following way
div2_ThenIncrement :: Int -> MaybeInt
div2_ThenIncrement = increment_ . div2_

-- If we want to avoid defining increment_ again only so that it can
-- receive and return an Option type (MaybeInt)
-- We could use something called a Functor
-- Functor is definded by having an fmap function which provides us with a way to solve our "problem"
instance Functor Option 
  where
    fmap :: (a -> b) -> Option a -> Option b
    fmap fn (Some x) = Some $ fn x
    fmap _ _ = None

-- Notice how we fmaped the increment function with the result of div2_ which is MaybeInt
div2_ThenIncrementWithFmap :: Int -> MaybeInt
div2_ThenIncrementWithFmap = fmap increment . div2_


sumMaybeInts :: MaybeInt -> MaybeInt -> MaybeInt
sumMaybeInts (Some x) (Some y) = Some (x + y)
sumMaybeInts _ _ = None

safeDiv :: Int -> Int -> MaybeInt
safeDiv _ 0 = None
safeDiv x y = Some $ div x y

safeHead :: [a] -> Option a
safeHead [] = None
safeHead (head:_) = Some head

safeSQRT :: Float -> Option Float
safeSQRT x
  | x < 0 = None
  | otherwise = Some (sqrt x)

-- Option is implemented in Haskell with the name Maybe
-- Just is Some
-- Nothing is None
type NativeMaybeInt = Maybe Int
nativeSafeDiv :: Int -> Int -> NativeMaybeInt
nativeSafeDiv _ 0 = Nothing
nativeSafeDiv x y = Just (div x y)
