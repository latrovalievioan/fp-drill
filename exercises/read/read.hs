import Data.List.Split
import qualified Data.Map.Strict as M

-- Read a file containing only words split by empty spaces
content = readFile "./input"

split_words = content >>= (\str -> return (splitOn " " str))

-- Count the occurancies of each word
wordCounters :: IO (M.Map String Int)
wordCounters = split_words >>= (\str -> return (foldl step M.empty str))
  where
    step :: M.Map String Int -> String -> M.Map String Int
    step acc curr = M.insertWith (+) curr 1 acc

-- Write a function that checks how much is the common
-- cardinality of the occurancies of two words
-- If one of the words doesn't occure atleast once return Nothing,
-- otherwise return Just the value
check2words :: M.Map String Int -> String -> String -> Maybe Int
check2words counters w1 w2 = w1Count >>= (\w1c -> w2Count >>= (\w2c -> return (w1c + w2c)))
  where
    w1Count = M.lookup w1 counters
    w2Count = M.lookup w2 counters

check2words' :: M.Map String Int -> String -> String -> Maybe Int
check2words' counters w1 w2 = ((+) <$> w1Count) <*> w2Count
  where
    w1Count = M.lookup w1 counters
    w2Count = M.lookup w2 counters

res = wordCounters >>= (\m -> return $ check2words m "earthquake" "disposition")
