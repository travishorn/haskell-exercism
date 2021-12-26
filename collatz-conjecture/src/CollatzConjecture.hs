-- The Collatz Conjecture or 3x+1 problem can be summarized as follows:

-- Take any positive integer n. If n is even, divide n by 2 to get n / 2. If n
-- is odd, multiply n by 3 and add 1 to get 3n + 1. Repeat the process
-- indefinitely. The conjecture states that no matter which number you start
-- with, you will always reach 1 eventually.

-- Given a number n, return the number of steps required to reach 1.

module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer

collatz x
  -- The Collatz Conjecture does not apply to numbers less than or equal to 0.
  -- If called with 0, return Nothing.
  | x <= 0 = Nothing

  -- If x is 1, there are no steps to perform. Every x will eventually get here.
  -- Return Just 0. This will be the base of the stack and act like an
  -- accumulator.
  | x == 1 = Just 0

  -- If x is even, map succ (aka (+1)) over the rest of the stack. This will
  -- have the effect of adding 1 to the "accumulator" for every time collatz is
  -- called. Call collatz again on x / 2.

  -- <$> is the infix form of fmap. This line is equivalent to
  -- fmap succ $ collatz (x `div` 2)

  -- Must use `div` instead of `/`. https://stackoverflow.com/q/7368926/
  | even x = succ <$> collatz (x `div` 2)

  -- Otherwise, x is odd. Do the same thing as even, but instead of dividing by
  -- 2, multiply by 3 and add 1.
  | otherwise = succ <$> collatz (x * 3 + 1)
