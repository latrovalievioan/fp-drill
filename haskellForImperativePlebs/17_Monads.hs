-- type Monad :: (* -> *) -> Constraint
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a
--   {-# MINIMAL (>>=) #-}
--   	-- Defined in ‘GHC.Base’
-- instance Monad (Either e) -- Defined in ‘Data.Either’
-- instance Monad [] -- Defined in ‘GHC.Base’
-- instance Monad Solo -- Defined in ‘GHC.Base’
-- instance Monad Maybe -- Defined in ‘GHC.Base’
-- instance Monad IO -- Defined in ‘GHC.Base’
-- instance Monad ((->) r) -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b, Monoid c) => Monad ((,,,) a b c)
--   -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b) => Monad ((,,) a b)
--   -- Defined in ‘GHC.Base’
-- instance Monoid a => Monad ((,) a) -- Defined in ‘GHC.Base’


-- If we look at the definition of the Monad typeclass,
-- we can see that we only need one operator definition for
-- something to be a Monad and that is >>= (bind)


-- The >>= (bind) operator

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- Just 1 >>= (\x -> Just x) ==> Just 1
-- Nothing >>= (\x -> Just x) ==> Nothing

maybeAdd :: Maybe Int -> Int -> Maybe Int
maybeAdd mx y = mx >>= (\x -> Just $ x + y)

maybiesAdd :: Maybe Int -> Maybe Int -> Maybe Int
maybiesAdd mx my = mx >>= (\x -> my >>= (\y -> Just $ x + y))

-- The return operator takes a favue and 
-- returns the value wrapped in the monadic context.
-- This means that we can use it like so:
maybiesAdd' :: Maybe Int -> Maybe Int -> Maybe Int
maybiesAdd' mx my = mx >>= (\x -> my >>= (\y -> return $ x + y))

-- Notice that if we have more monadic arguments
-- we have to use the bind operator for each of them
-- this causes our code to be fairly unreadable,
-- to fix this we can use the do notation like so:

doMaybiesAdd :: Maybe Int -> Maybe Int -> Maybe Int
doMaybiesAdd mx my = do
  x <- mx
  y <- my

  return $ x + y

-- Let's implement Option and a Monad instance for it
data Option a = None | Some a deriving (Show, Eq)

instance Functor Option where
  fmap :: (a -> b) -> Option a -> Option b
  fmap fn (Some x) = Some (fn x)
  fmap _ None = None

instance Applicative Option where
  pure :: a -> Option a
  pure = Some

  (<*>) :: Option (a -> b) -> Option a -> Option b
  (<*>) (Some fn) (Some x) = Some $ fn x
  (<*>) _ _ = None

instance Monad Option where
  (>>=) :: Option a -> (a -> Option b) -> Option b
  (>>=) None _ = None
  (>>=) (Some x) f = f x

testBind :: Option Int -> Option Int -> Option Int
testBind ox oy = do
  x <- ox
  y <- oy
  return $ x + y

testApp :: Option Int -> Option Int -> Option Int
testApp ox oy = (+) <$> ox <*> oy

testFunc :: Option Int -> Option String
testFunc ox = (\x -> show $ x + 1) <$> ox

main = do
  print [testBind (Some 3) (Some 4), testBind None (Some 3), testBind (Some 3) None, testBind None None]
  print [testApp (Some 3) (Some 4), testApp None (Some 3), testApp (Some 3) None, testApp None None]
  print [testFunc (Some 3), testFunc None]
