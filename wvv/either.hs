-- Sometimes we want to provide a feedback on why a value doesn't exist
-- This cannot be done with the Option/Maybe type because the return type of
-- a non existing value is None/Nothing
-- 
-- Consider the following example:
data Option a = Some a | None deriving Show

safeDiv :: Int -> Int -> Option Int
safeDiv _ 0 = None
safeDiv x y =  Some (div x y)
-- Notice that we don't get any information why our division failed.

-- Now check out how we can get information on what went wrong:
data BadOrGood a b = Bad a | Good b deriving Show

safeDiv_ :: Int -> Int -> BadOrGood String Int
safeDiv_ _ 0 = Bad "Division by zero is not allowed."
safeDiv_ x y = Good (div x y)

-- BadOrGood is implemented in Haskell with the name Either
-- Good is Right
-- Bad is Left
nativeSafeDiv :: Int -> Int -> Either String Int
nativeSafeDiv _ 0 = Left "Division by zero is not allowed."
nativeSafeDiv x y = Right (div x y)

-- Let's define our beloved increment function
increment :: Int -> Int
increment = (+1)

-- Now let's try to compose it with safeDiv_ which uses our BadOrGood data type
-- Variant 1 is to extend the increment function to receive a BadOrGood value and return a BadOrGood value
increment_ :: BadOrGood String Int -> BadOrGood String Int
increment_ (Bad s) = Bad s
increment_ (Good i) = Good (i + 1)

safeDivThenIncrement :: Int -> Int -> BadOrGood String Int
safeDivThenIncrement x y = increment_ (safeDiv_ x y)

-- Variant 2 is to make BadOrGood a Functor
-- which means to implement an fmap function for it similar with what we did with our option type
instance Functor (BadOrGood a)
  where
    fmap :: (b -> c) -> BadOrGood a b -> BadOrGood a c
    fmap fn (Good x) = Good (fn x)
    fmap _ (Bad x) = Bad x

-- Now we can compose them:
safeDivThenIncrement_ :: Int -> Int -> BadOrGood String Int
safeDivThenIncrement_ x y = fmap increment (safeDiv_ x y)
