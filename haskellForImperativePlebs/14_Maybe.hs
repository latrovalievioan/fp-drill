import Data.Maybe
-- Maybe has one polymorphic type "a"
-- and consists of two constructors Nothing and Just a
data Maybe' a = Nothing' | Just' a

-- We need Maybe because all functions in Haskell should
-- produce an output, but what if there is some kind of an
-- error in our function, for example dividing by zero
-- this is where maybe comes handy, we can implement a 
-- safe division function the following way
safeDiv :: Integral a => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just $ div x y

-- Maybe functions:
isJust' :: Maybe a -> Bool
isJust' = undefined
-- checks if a value is just.

isNothing' :: Maybe a -> Bool
isNothing' = undefined
-- checks if a value is nothing.

fromJust' :: Maybe a -> a
fromJust' = undefined
-- extracts the value from a Just constructor
-- NB: throws an exception if we try to use it with Nothing

fromMaybe' :: a -> Maybe a -> a
fromMaybe' = undefined
-- takes a default value as a first param
-- then takes a Maybe type
-- if the Maybe is Nothing returns the default value
-- if the Maybe is Just it extracts the value from Just and returns it

unwrapSafeDiv :: Integral a => Maybe a -> a
unwrapSafeDiv = fromMaybe 0

safeDivThenUnwrap :: Integral a => a -> a -> a
safeDivThenUnwrap n m = unwrapSafeDiv $ safeDiv n m
