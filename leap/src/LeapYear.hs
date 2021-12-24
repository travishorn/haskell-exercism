-- Given a year, report if it is a leap year.

-- The tricky thing here is that a leap year in the Gregorian calendar occurs:
-- on every year that is evenly divisible by 4
--   except every year that is evenly divisible by 100
--     unless the year is also evenly divisible by 400

-- For example, 1997 is not a leap year, but 1996 is. 1900 is not a leap year,
-- but 2000 is.

-- Another way to put it:
-- Is year divisible by 400? It is a leap year.
-- If not, is year divisible by 100? It is NOT a leap year.
-- If not, is year divisible by 4? It is a leap year.
-- If not, it is NOT a leap year.

module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool

-- Using "guards". Haskell "goes down the list" until it finds a matching case.
-- First we check if it's divisible by 400, then 100, then 4, returning True or
-- False when appropriate
isLeapYear year
  | isDivisibleBy 400 = True
  | isDivisibleBy 100 = False
  | isDivisibleBy   4 = True
  | otherwise         = False

-- But `isDivisibleBy` isn't a built in function, instead we define it here at
-- the end of the guard. You can see it has access to the year that was passed
-- in.

-- Takes a value `x` (400 in the first case) and the passed in year. Calculates
-- the modulus of year and x. If it equals 0, the case is a match and that
-- expression is chosen.

  where isDivisibleBy x = mod year x == 0

-- If you've never worked with `mod` or done `modulus` math before, you don't
-- need to know *exactly* how it works. Just know that if a number is evenly
-- divisible by another number, it's modulus will be 0.
-- Examples:
-- 2020 mod 400 does NOT equal 0 because 2020 is not evenly divisible by 400
-- 2020 mod 100 does NOT equal 0 because 2020 is not evenly divisible by 100
-- 2020 mod 4 = 0 because 2020 is evenly divisible by 4
-- So the first case to match in the `isLeapYear` function would be
-- `isDivisibleBy 4`. The function would return True. 2020 is a leap year.
