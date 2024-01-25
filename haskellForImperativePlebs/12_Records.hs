-- The following datatype representing a person has a constructor
-- that expects the name and the age of the person
-- but currently we are representing them only by types
data Person = Person String Int

-- There is a special datatype in Haskell called Records
-- that can help use achive what we need in this case
data Person' = Person' { name :: String,
                         age :: Int }
-- Notice how the types are the same but the arguments are named

-- From the definition above there are two functions
-- that are generated automatically:
-- name :: Person' -> String
-- age :: Person' -> Int

greet :: Person' -> String
greet person = "Hi" ++ name person
