import GHC.IO (unsafePerformIO)
-- We will finally write "Hello World" in Haskell
res = putStrLn "Hello World"
-- Notice how we can save the result from the putStrLn operation in a
-- variable. Notice also how the type of res variable is IO ().
res :: IO ()
--
-- The "()" (empty tuple) is a special type which only has one value,
-- and that is the empty tuple.
-- It is used to signal that we have "no information" in that place

-- The Hello World string is printed only after we call the "res" function
-- for evaluation.
--
-- The res function has seamingly no input, it has an output of an empty tuple,
-- but it also has an interaction with the environment this makes it an IO action
-- instead of a function since it isn't pure.

-- Let's take a look at another IO action called getLine
-- getLine reads one line from the standard input
line :: IO String
line = getLine

-- Notice the type of line is a String wrapped in some IO
-- this means that we cannot use it as a normal string
-- for example we cannot concat it with a String:
-- newLine = line ++ "some random string"

-- To be able to use our IO String as a normal one,
-- we could use the "do" notation to extract it from the IO

readWrite :: IO ()
readWrite = do
  line <- getLine
  putStrLn $ line ++ " some random string"

-- Notice how in order to extract our line as a String from the IO String,
-- we use the "<-" syntax. Keep in mind this syntax is only valid inside a 
-- "do" block.

-- There is a function in haskell that can act as the "<-" operator
-- outside of a "do" block. It is called unsafePerformIO and it's type is:
fn :: IO a -> a
fn = undefined
-- There us a reason why it is called "unsafe". And the reason is that
-- it shouldn't be used.
-- The unsafePerformIO disregards the environment that we encapsulate within
-- IO and this can lead to all sorts of unexpected behaviour

----------------- DON'T DO THIS ----------------- 
line' :: IO String
line' = getLine

newLine = unsafePerformIO line' ++ " some rendom string"
----------------- DON'T DO THIS ----------------- 

-- Most of the time Main functions in Haskell are not 
-- functions, but IO actions
main :: IO ()
main = do
  l <- getLine
  if l /= "quit" then do
    putStrLn $ "Input:" ++ l
    main
  else
    return ()
-- Returning means that we encapsulate a value in the Action (IO in this case)
-- We encapsulate the () (empty tuple) inside IO which gives use the right
-- return type "IO ()"
