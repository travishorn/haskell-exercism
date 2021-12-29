-- Given a number, find the sum of all the unique multiples of particular
-- numbers up to but not including that number.

-- If we list all the natural numbers below 20 that are multiples of 3 or 5, we
-- get 3, 5, 6, 9, 10, 12, 15, and 18.

-- The sum of these multiples is 78.

module SumOfMultiples (sumOfMultiples) where

-- Use a filter to remove 0s from a list of integers. For example, [0,1,0,2]
-- would return [1,2].
removeZeros :: [Integer] -> [Integer]
removeZeros = filter (/= 0)

-- Given two numbers, is the first a multple of the second? True/False
-- `mod` is the modulo operator. If x mod y = 0, x can be divided by y evenly.
-- In other words, y is a multiple of x.
isMultipleOf :: Integer -> Integer -> Bool
isMultipleOf x y = x `mod` y == 0

-- Given a list of integers xs and another integer y, determine whether y is a
-- multiple of any of the xs. If y is an integer of any of xs, True. Otherwise
-- False.
isMultipleOfAny :: [Integer] -> Integer -> Bool
isMultipleOfAny xs y = any (isMultipleOf y) xs

-- Use the isMultipleOfAny function above to filter a list of integers xs. The
-- only integers left in the returned list after filtering are multiples of at
-- least one of the ys.
onlyMultiplesOf :: [Integer] -> [Integer] -> [Integer]
onlyMultiplesOf xs ys = filter (isMultipleOfAny ys) xs

-- The solution.
sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =

  -- The sum of...
  sum $

  -- the multiples (from 1 to the limit) of...
  onlyMultiplesOf [1..limit-1] $

  -- the list of factors (without zeros)
  removeZeros factors
