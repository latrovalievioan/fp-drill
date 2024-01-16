-- Functional Programming
-- - Pure (mathematical) functions
-- - Immutable data
-- - No/Less side effects
-- - Declarative
-- - Easier to verify (we can proove mathematically our code)

-- Haskell is lazy evaluated
-- Consider the following example:
fn1 :: a -> Bool
fn1 = undefined

fn2 :: a -> Bool
fn2 = undefined

fn3 :: a -> Bool
fn3 = undefined

-- In testFn there wont be any evaluation for fn1, fn2 or fn3 before the if statement
-- when the if statement is executed first "z" will be evaluated, then if "z" is True
-- "x" will be evaluated, but if "z" is False, the compiler will skip to "y"'s evaluation
testFn :: a -> Bool
testFn arg = 
  let x = fn1 arg
      y = fn2 arg
      z = fn3 arg
  in
  if z then x else y

