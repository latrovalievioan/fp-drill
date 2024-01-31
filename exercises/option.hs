-- Implement an Option typeclass that is a Monad

increment :: Int -> Int
increment = (+1)

data Option a = None | Some a deriving Show

safeDiv :: Int -> Int -> Option Int
safeDiv _ 0 = None
safeDiv x y = Some $ div x y

instance Functor Option where
  fmap :: (a -> b) -> Option a -> Option b
  fmap fn (Some x) = Some $ fn x
  fmap _ None = None

safeDivThenIncrement :: Int -> Int -> Option Int
safeDivThenIncrement _ 0 = None
safeDivThenIncrement x y = increment <$> safeDiv x y

instance Applicative Option where
  pure :: a -> Option a
  pure = Some

  (<*>) :: Option (a -> b) -> Option a -> Option b
  (<*>) (Some fn) (Some x) = Some (fn x)
  (<*>) _ _ = None
