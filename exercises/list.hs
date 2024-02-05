-- Implement a List that is a Monad in Haskell

data Ls a = Empty | a :> (Ls a)

oing :: Show a => String -> Ls a -> String
oing _ Empty = ""
oing _ (x :> Empty) = show x
oing sep (x :> xs) = show x ++ sep ++ oing sep xs

instance Show a => Show (Ls a) where
  show :: Ls a -> String
  show Empty = "{}"
  show xs = "{" ++ oing "," xs ++ "}"
  

cat :: Ls a -> Ls a -> Ls a
cat Empty ys = ys
cat xs Empty = xs
cat (x :> xs) ys = x :> cat xs ys

instance Functor Ls where
  fmap :: (a -> b) -> Ls a -> Ls b
  fmap _ Empty = Empty
  fmap fn (x :> xs) = fn x :> fmap fn xs

instance Applicative Ls where
  (<*>) :: Ls (a -> b) -> Ls a -> Ls b
  (<*>) (fn :> fns) (x :> xs) = fn x :> (fns <*> xs)
  (<*>) _ _ = Empty

  pure :: a -> Ls a 
  pure x = x :> Empty

instance Monad Ls where
  (>>=) :: Ls a -> (a -> Ls b) -> Ls b
  (>>=) (x :> xs) fn = cat (fn x) (xs >>= fn)
  (>>=) _ _ = Empty


main = do
  let list = 1 :> (2 :> (3 :> Empty))
  
  print "Functor:"
  let listTimesTwo = (*2) <$> list
  print listTimesTwo

  print "Applicative:"
  let fns = (+) <$> list
  print (fns <*> list)

  print "Monad:"
  print $ [1,2,3] >>= (\x -> 42 : [1,2,3])
  print $ list >>= (\x -> 42 :> list)
