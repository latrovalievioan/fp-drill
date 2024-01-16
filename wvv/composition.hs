increment :: Int -> Int
increment x = x + 1

toString :: Int -> String
toString x = show x


incremented = increment 4
stringified = toString incremented

-- same as

stringified_ = toString (increment 4)

-- cant we just combine increment with toString to get a function int ->increment-tostring-> string
-- lets define a way to compose functions
compose :: (a -> b) -> (b -> c) -> a -> c
compose fn1 fn2 v = fn2(fn1 v)

incrementThenToString = compose increment toString

-- lets define our own operator just for the drill
(<..>) :: (a -> b) -> (b -> c) -> a -> c
(fn1 <..> fn2) v = fn2(fn1 v) 
incrementThenToString2 = increment <..> toString

-- Haskell has a (smooth) operator for composing functions: "."
incrementThenToString3 = toString . increment
