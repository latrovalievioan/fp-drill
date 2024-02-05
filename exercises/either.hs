-- Implement an Either typeclass that is a Monad

increment :: Int -> Int
increment = (+ 1)

data Either' l r = Left' l | Right' r deriving Show

safeDivWithError :: Int -> Int -> Either' String Int
safeDivWithError _ 0 = Left' "Cannot divide by 0"
safeDivWithError x y = Right' $ div x y

instance Functor (Either' l) where
  fmap :: (a -> b) -> Either' l a -> Either' l b
  fmap _ (Left' str) = Left' str
  fmap fn (Right' val) = Right' $ fn val

safeDivThenIncrement :: Int -> Int -> Either' String Int
safeDivThenIncrement x y = increment <$> safeDivWithError x y

instance Applicative (Either' l) where
  (<*>) :: Either' l (a -> b) -> Either' l a -> Either' l b
  (<*>) (Right' fn) (Right' val) = Right' $ fn val
  (<*>) (Left' str) _ = Left' str
  (<*>) _ (Left' str) = Left' str

  pure :: a -> Either' l a
  pure = Right'

sumEithers :: Either' String Int -> Either' String Int -> Either' String Int
sumEithers ex ey = (+) <$> ex <*> ey

instance Monad (Either' l) where
  (>>=) :: Either' l a -> (a -> Either' l b) -> Either' l b
  (>>=) (Left' str) _ = Left' str
  (>>=) (Right' val) fn = fn val

sumEithersWithBind :: Either' String Int -> Either' String Int -> Either' String Int
sumEithersWithBind ex ey = ex >>= (\x -> ey >>= (\y -> return (x + y)))

sumEithersWithDo :: Either' String Int -> Either' String Int -> Either' String Int
sumEithersWithDo ex ey = do
  x <- ex
  y <- ey
  return (x + y)

main :: IO ()
main = do
  print "Either"
  print $ safeDivWithError 8 4
  print $ safeDivWithError 8 0
  print "Functor:"
  print $ safeDivThenIncrement 8 4
  print $ safeDivThenIncrement 8 0
  print "Applicative:"
  print $ sumEithers (safeDivWithError 8 4) (safeDivWithError 6 3)
  print $ sumEithers (safeDivWithError 8 0) (safeDivWithError 6 3)
  print $ sumEithers (safeDivWithError 8 4) (safeDivWithError 6 0)
  print "Monad:"
  print $ sumEithersWithBind (safeDivWithError 8 4) (safeDivWithError 6 3)
  print $ sumEithersWithBind (safeDivWithError 8 0) (safeDivWithError 6 3)
  print $ sumEithersWithBind (safeDivWithError 8 4) (safeDivWithError 6 0)
  print "Monad DO:"
  print $ sumEithersWithDo (safeDivWithError 8 4) (safeDivWithError 6 3)
  print $ sumEithersWithDo (safeDivWithError 8 0) (safeDivWithError 6 3)
  print $ sumEithersWithDo (safeDivWithError 8 4) (safeDivWithError 6 0)
